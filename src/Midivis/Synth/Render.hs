{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
{-# LANGUAGE BangPatterns #-}
module Midivis.Synth.Render where

import Prelude
import Control.Monad (forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray)
import Data.IORef (IORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, peekByteOff, pokeByteOff, peekElemOff, pokeElemOff)
import Foreign.ForeignPtr (withForeignPtr)
import Data.Ord (clamp)

import Midivis.Synth.VoicePool
import Midivis.Synth.Types
import Midivis.Synth.ConvReverb (ConvReverb, processReverb)
import Midivis.Util.Math
import Data.Bits (shiftR, (.&.))
import System.Random (randomRIO)


-- | Deterministic smooth-random LFO value in [-1, 1] at absolute sample
--   position @sampleCount@.  Sample-and-hold segments at @rate@ Hz joined by
--   linear interpolation; the hash is a wrapping Int multiply so no heap
--   allocation happens in the audio callback.
smoothNoise :: Double -> Double -> Double -> Int -> Double
smoothNoise sampleCount sr rate seed =
    let pos  = sampleCount / sr * rate
        seg  = floor pos :: Int
        frac = pos - fromIntegral seg
        v0   = noiseAt seg
        v1   = noiseAt (seg + 1)
    in v0 + (v1 - v0) * frac
  where
    noiseAt :: Int -> Double
    noiseAt s =
        let h = (fromIntegral s * 2654435761 + fromIntegral seed * 40503) * 1103515245 :: Int
            u = fromIntegral ((h `shiftR` 16) .&. 0xFFFFFF) :: Double
        in u / 8388607.5 - 1.0

-- | Render all active voices directly into a PortAudio output buffer.
--   Bulk-reads VoicePool once, renders audio, advances phase per voice,
--   then convolution reverb (wet), then a master compressor.
--   @ampSmoothRef@ / @ampMidRef@ hold per-voice two-stage smoothed amplitude
--   (IOUArrays, 128 slots) so aftertouch glides instead of clicking.
renderCallback :: VoicePool -> Double -> Int -> Ptr CFloat -> IORef Double -> IORef ConvReverb -> IORef Double -> IORef (IOUArray Int Double) -> IORef (IOUArray Int Double) -> IO ()
renderCallback vp sr nFrames outPtr gainRef reverbRef smpRef ampSmoothRef ampMidRef = do
    forM_ [0 .. totalSamples - 1] $ \i ->
        pokeElemOff outPtr i (0.0 :: CFloat)

    -- Random LFOs (constant within this block, smooth across blocks)
    t0 <- readIORef smpRef
    writeIORef smpRef (t0 + fromIntegral nFrames)
    let !lfo1 = smoothNoise t0 sr defLFORate1 defLFOSeed1
        !lfo2 = smoothNoise t0 sr defLFORate2 defLFOSeed2
        !lfo3 = smoothNoise t0 sr defLFORate3 defLFOSeed3
        !m1   = 1.0 + lfo1 * defLFODepth1      -- fundamental: small wobble
        !m2   = 1.0 + lfo2 * defLFODepth2      -- overtone: deeper wobble
        !m3   = 1.0 + lfo3 * defLFODepth3

    ampSmoothArr <- readIORef ampSmoothRef
    ampMidArr    <- readIORef ampMidRef
    let !n = nFrames :: Int
        !bT = fromIntegral nFrames / realToFrac sr :: Float
    let VoicePool fp = vp
    withForeignPtr fp $ \base -> do
        let readSlot i = peek (base `plusPtr` (i * 32)) :: IO VoiceParams
        forM_ [0 .. 127] $ \slot -> do
            VoiceParams freq amp ph env <- readSlot slot
            when (freq /= freqIdleSentinel && env /= Idle) $ do
                let !step = freq / sr :: Double

                -- Two-stage cascade smoothing: s1 (fast stage) chases the
                -- target, s2 (output) chases s1.  The state is updated
                -- unconditionally so the output keeps gliding toward the
                -- target even during fast envelope decays; rendering (and the
                -- per-block ramp) only runs while anything is audible.
                s2 <- unsafeRead ampSmoothArr slot
                s1 <- unsafeRead ampMidArr slot
                -- s1 (fast stage) always tracks the target quickly so it never
                -- holds a stale lag; only the output stage s2 uses the slow
                -- aftertouch smoothing, and only while sustaining.  During
                -- envelope stages s2 also tracks quickly so the voice is
                -- inaudible before it is dropped — no stuck level, no click.
                let !k1     = 0.5
                    !k2     = case env of
                                Sustain _ -> defAmpSmoothK
                                _         -> 0.5
                    !s1'    = s1 + k1 * (amp - s1)
                    !s2'    = s2 + k2 * (s1' - s2)
                    !audible = amp /= 0 || s2 > 0.0001
                when audible $ do
                    let !delta = (s2' - s2) / fromIntegral nFrames
                    forM_ [0 .. n - 1] (\f -> do
                        noise <- randomRIO (-0.01, 0.01)
                        let !sf = fromIntegral f :: Double
                            !af = s2 + delta * fromIntegral (f + 1)
                            !an = af * defNormalizeAmp
                            !s  = realToFrac ((tanh) ( --with spectral amp adjustments
                                m1 * sin (2 * pi * (ph + sf * step)) * 0.8  * (-0.22 * (log10 freq) + 1.2) +
                                m2 * sin (4 * pi * (ph + sf * step)) * 0.2 * an * (-0.14 * (log10 freq) + 1.14) +
                                m3 * sin (6 * pi * (ph + sf * step)) * 0.514 * an * an +
                                m1 * m2 * m3 * noise
                                ) * an) :: CFloat
                        curL <- peekElemOff outPtr (f * 2)
                        pokeElemOff outPtr (f * 2)     (curL + s)
                        curR <- peekElemOff outPtr (f * 2 + 1)
                        pokeElemOff outPtr (f * 2 + 1) (curR + s))
                unsafeWrite ampSmoothArr slot s2'
                unsafeWrite ampMidArr slot s1'

                let !newPh = ph + fromIntegral n * step
                -- Envelope advance.  Every env write is guarded by a re-read:
                -- the MIDI thread (mainLoop) may have just triggered
                -- Attack/Release/Idle on this voice, and overwriting it here
                -- would either keep a released note alive or cut a new one —
                -- both sound as clicks.  If the field changed since our block
                -- snapshot, skip the write and let the new state win.
                let guardWrite act = do
                        curEnv <- peek (base `plusPtr` (slot * 32 + 24)) :: IO EnvelopeState
                        when (curEnv == env) act
                case env of
                    Attack d c
                        | d <= bT -> guardWrite $ do
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  (1.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 defDecay
                        | otherwise -> guardWrite $ do
                            let !newD = d - bT
                                !newA = amp + deltaPower amp 1.0 (realToFrac c) (realToFrac (bT / d)) amp
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  newA
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 (Attack newD c)
                    Decay d c
                        | d <= bT -> guardWrite $ do
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  defSustainLevel'
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 defSustain
                        | otherwise -> guardWrite $ do
                            let !newD = d - bT
                                !newA = amp + deltaPower amp defSustainLevel' (realToFrac c) (realToFrac (bT / d)) amp
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  newA
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 (Decay newD c)
                    Sustain _ -> do
                        -- Keep the current amp (aftertouch controls it);
                        -- Decay already landed on defSustainLevel'.
                        pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                    Release d c
                        | s2' < 0.0005 -> guardWrite $ do
                            -- The smoothed output is fully inaudible: drop.
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 (0.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  (0.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 Idle
                        | otherwise -> guardWrite $ do
                            -- Curve decay while d remains; once spent, keep an
                            -- exponential fade-out (-50%/block).  Cutting at a
                            -- non-zero amp clicks, and with a slow smoothing
                            -- constant the output can still be loud when the
                            -- envelope target reaches 0 — so the Idle gate
                            -- above is driven by the actual smoothed output.
                            let !newD = max 0 (d - bT)
                                !newA = if d > bT
                                            then clamp (0, 1) $ amp + deltaPower amp 0.0 (realToFrac c) (realToFrac (bT / d)) amp
                                            else amp * 0.5
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  newA
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 (Release newD c)
                    Idle -> return ()

    -- Convolution reverb (wet path) — after mixing, before the compressor
    processReverb reverbRef nFrames outPtr

    -- Master compressor post-pass
    compressBuffer outPtr totalSamples sr gainRef
  where
    totalSamples = nFrames * 2

-- | Feed-forward compressor post-pass (stereo-linked gain).
compressBuffer :: Ptr CFloat -> Int -> Double -> IORef Double -> IO ()
compressBuffer outPtr totalSamples sr gainRef = do
    g <- readIORef gainRef
    let thr    = fromDBFS (-6.0)
        ratio  = 100.0
        aAtk   = 1.0 - exp (-1.0 / (0.001 * sr))    -- 1 ms attack
        aRel   = 1.0 - exp (-1.0 / (0.325 * sr))    -- 325 ms release
        go !i !gain
            | i >= totalSamples = writeIORef gainRef gain
            | otherwise = do
                xL <- peekElemOff outPtr i
                xR <- peekElemOff outPtr (i + 1)
                let env = max (abs (realToFrac xL)) (abs (realToFrac xR)) :: Double
                    targetGain
                        | env > thr  = (thr + (env - thr) / ratio) / env
                        | otherwise  = 1.0
                    alpha  = if targetGain < gain then aAtk else aRel
                    gain'  = gain + alpha * (targetGain - gain)
                    yL     = realToFrac (realToFrac xL * gain') :: CFloat
                    yR     = realToFrac (realToFrac xR * gain') :: CFloat
                pokeElemOff outPtr i     yL
                pokeElemOff outPtr (i + 1) yR
                go (i + 2) gain'
    go 0 g

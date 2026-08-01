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
import Foreign.Storable (peek, pokeByteOff, peekElemOff, pokeElemOff)
import Foreign.ForeignPtr (withForeignPtr)
import Data.Ord (clamp)

import Midivis.Synth.VoicePool
import Midivis.Synth.Types
import Midivis.Synth.ConvReverb (ConvReverb, processReverb)
import Midivis.Synth.AlgorithmicReverb (AlgReverb, processAlgReverb)
import Midivis.Util.Math
import Data.Bits (shiftR, (.&.))


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

-- | Deterministic white-ish noise in [-0.01, 0.01] for an absolute sample
--   position and voice slot.  Replaces per-sample 'randomRIO' in the real-time
--   callback: the global StdGen is not safe to hammer ~262k times per callback
--   (it would flood the GC with split allocations and race with the MIDI
--   thread), and the callback path must stay allocation-free.
detNoise :: Int -> Int -> Double
detNoise s slot =
    let h = (fromIntegral s * 2654435761 + fromIntegral (slot * 7919 + 17) * 40503) * 1103515245 :: Int
        u = fromIntegral ((h `shiftR` 16) .&. 0xFFFFFF) :: Double
    in (u / 8388607.5 - 1.0) * 0.01

-- | Render all active voices directly into a PortAudio output buffer.
--   Bulk-reads VoicePool once, renders audio, advances phase per voice,
--   then algorithmic reverb (Freeverb), convolution reverb (wet), then a
--   master compressor.  @algRef@ holds the tweakable pre-reverb; @widthRef@
--   the stereo-widener allpass state.
--   @ampSmoothRef@ / @ampMidRef@ hold per-voice two-stage smoothed amplitude
--   (IOUArrays, 128 slots) so aftertouch glides instead of clicking.
renderCallback :: VoicePool -> Double -> Int -> Ptr CFloat -> IORef Double -> IORef ConvReverb -> IORef AlgReverb -> IORef Double -> IORef (IOUArray Int Double) -> IORef (IOUArray Int Double) -> IORef (Double, Double) -> IO ()
renderCallback vp sr nFrames outPtr gainRef reverbRef algRef smpRef ampSmoothRef ampMidRef widthRef = do
    -- 1. Zero the interleaved output buffer; every voice accumulates into it.
    forM_ [0 .. totalSamples - 1] $ \i ->
        pokeElemOff outPtr i (0.0 :: CFloat)

    -- 2. Block-constant LFOs: sampled at the block start, so the modulation
    --    is continuous across blocks but constant within one (no zipper).
    t0 <- readIORef smpRef
    writeIORef smpRef (t0 + fromIntegral nFrames)
    let !lfo1 = smoothNoise t0 sr defLFORate1 defLFOSeed1
        !lfo2 = smoothNoise t0 sr defLFORate2 defLFOSeed2
        !lfo3 = smoothNoise t0 sr defLFORate3 defLFOSeed3
        !m1   = 1.0 + lfo1 * defLFODepth1      -- fundamental: small wobble
        !m2   = 1.0 + lfo2 * defLFODepth2      -- overtone: deeper wobble
        !m3   = 0.79 + lfo3 * defLFODepth3      -- 3rd/4th harmonic: deepest

    ampSmoothArr <- readIORef ampSmoothRef
    ampMidArr    <- readIORef ampMidRef
    let !n = nFrames :: Int
        !bT = fromIntegral nFrames / realToFrac sr :: Float
    let VoicePool fp = vp
    -- 3. Per-voice loop: snapshot all 128 slots, render the audible ones.
    withForeignPtr fp $ \base -> do
        let readSlot i = peek (base `plusPtr` (i * 32)) :: IO VoiceParams
        forM_ [0 .. 127] $ \slot -> do
            VoiceParams freq amp ph env <- readSlot slot
            when (freq /= freqIdleSentinel && env /= Idle) $ do
                let !step = freq / sr :: Double

                -- 3a. Two-stage cascade smoothing: s1 (fast stage) chases the
                -- target, s2 (output) chases s1.  The state is updated
                -- unconditionally so the output keeps gliding toward the
                -- target even during fast envelope decays; rendering (and the
                -- per-block ramp) only runs while anything is audible.
                s2 <- unsafeRead ampSmoothArr slot
                s1 <- unsafeRead ampMidArr slot
                -- s1 always tracks the target quickly so it never holds a
                -- stale lag; only the output stage s2 uses the slow aftertouch
                -- smoothing, and only while sustaining.  During envelope
                -- stages s2 also tracks quickly so the voice is inaudible
                -- before it is dropped — no stuck level, no click.
                let !k1     = 0.5
                    !k2     = case env of
                                Sustain _ -> defAmpSmoothK
                                _         -> 0.5
                    !s1'    = s1 + k1 * (amp - s1)
                    !s2'    = s2 + k2 * (s1' - s2)
                    !audible = amp /= 0 || s2 > 0.0001
                when audible $ do
                    -- 3b. Sample loop: linear ramp s2 → s2' across the block
                    -- (block boundary stays continuous, no click).
                    let !delta = (s2' - s2) / fromIntegral nFrames
                    forM_ [0 .. n - 1] (\f -> do
                        let !sf = fromIntegral f :: Double
                            !noise = detNoise (fromIntegral (round t0) + f) slot
                            !af = s2 + delta * fromIntegral (f + 1)
                            !an = af * defNormalizeAmp
                            -- Per-sample waveform: fundamental + 2nd/3rd/4th
                            -- harmonics.  Each partial carries a log-frequency
                            -- tilt compensation so high notes don't overload;
                            -- m1/m2/m3 are the LFO modulators, the 4th
                            -- harmonic is FM'd by the deterministic noise,
                            -- and the sum goes through tanh before the final
                            -- -9dBFS per-voice gain.
                            !s  = realToFrac (tanh ( --with spectral amp adjustments
                                m1 * sin (2 * pi * (ph + sf * step)) * 0.8  * (- 0.22 * log10 freq + 1.2) +
                                m2 * sin (4 * pi * (ph + sf * step)) * 0.02 * an * (- 0.34 * log10 freq + 1.34) +
                                m3 * sin (6 * pi * (ph + sf * step)) * 0.314 * an * (- 0.17 * log10 freq + 1.17) +
                                m2 * sin (6 * pi * (ph + sf * step) * (noise + 1)) * 0.0114 * an * an * an
                                ) * an) :: CFloat
                        -- Accumulate into both channels (stereo, same signal).
                        curL <- peekElemOff outPtr (f * 2)
                        pokeElemOff outPtr (f * 2)     (curL + s)
                        curR <- peekElemOff outPtr (f * 2 + 1)
                        pokeElemOff outPtr (f * 2 + 1) (curR + s))
                unsafeWrite ampSmoothArr slot s2'
                unsafeWrite ampMidArr slot s1'

                -- 4. Envelope advance.  Every env write is guarded by a
                --    re-read: the MIDI thread (mainLoop) may have just
                --    triggered Attack/Release/Idle on this voice, and
                --    overwriting it here would either keep a released note
                --    alive or cut a new one — both sound as clicks.  If the
                --    field changed since our block snapshot, skip the write
                --    and let the new state win.
                let !newPh = ph + fromIntegral n * step
                    guardWrite act = do
                        curEnv <- peek (base `plusPtr` (slot * 32 + 24)) :: IO EnvelopeState
                        when (curEnv == env) act
                case env of
                    -- Attack: ramp amp toward 1.0; when the stage duration is spent,
                    -- hand over to Decay at full level.
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
                    -- Decay: glide toward the sustain level, then settle into Sustain.
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
                    -- Sustain: keep the current amp — aftertouch controls it from here
                    -- on.  (Decay already landed on defSustainLevel'; writing the level
                    -- every block would fight aftertouch=0 and cause clicks.)
                    Sustain _ -> do
                        pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                    -- Release: curve decay while d remains; once spent, keep an
                    -- exponential fade-out (-50%/block) instead of hard-cutting.  The
                    -- Idle gate below is driven by the *actual smoothed output* s2', not
                    -- the envelope target — with a slow smoothing constant the output can
                    -- still be loud when the target reaches 0, and cutting then clicks.
                    Release d c
                        | s2' < 0.0005 -> guardWrite $ do
                            -- Fully inaudible: drop the voice.
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 (0.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  (0.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 Idle
                        | otherwise -> guardWrite $ do
                            let !newD = max 0 (d - bT)
                                !newA = if d > bT
                                            then clamp (0, 1) $ amp + deltaPower amp 0.0 (realToFrac c) (realToFrac (bT / d)) amp
                                            else amp * 0.5
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  newA
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 (Release newD c)
                    Idle -> return ()

    -- 5. Dry-path stereo widening: R channel through a first-order allpass —
    --    its phase vs. L now varies with frequency → interaural width.  L
    --    stays untouched; defStereoWidth = 0 disables (L=R as before).
    when (defStereoWidth > 0) $
        widenR widthRef totalSamples outPtr

    -- 6. Algorithmic reverb (Freeverb) — tweakable space enhancer
    processAlgReverb algRef nFrames outPtr

    -- 7. Convolution reverb (wet path) — after mixing, before the compressor
    processReverb reverbRef nFrames outPtr

    -- 8. Master compressor post-pass
    compressBuffer outPtr totalSamples sr gainRef
  where
    totalSamples = nFrames * 2

-- | First-order allpass on the R channel only: y = a·x + x1 − a·y1.
--   State (prev input, prev output) persists across blocks in @stRef@.
widenR :: IORef (Double, Double) -> Int -> Ptr CFloat -> IO ()
widenR stRef totalSamples outPtr = do
    (x1, y1) <- readIORef stRef
    let a = defStereoWidth
        go !i !xp !yp
            | i >= totalSamples = writeIORef stRef (xp, yp)
            | otherwise = do
                x <- peekElemOff outPtr (i + 1)
                let y = a * realToFrac x + xp - a * yp
                pokeElemOff outPtr (i + 1) (realToFrac y)
                go (i + 2) (realToFrac x) y
    go 0 x1 y1

-- | Feed-forward compressor post-pass (stereo-linked gain).
--   Detects the peak envelope of both channels, derives a target gain
--   (1.0 below threshold, heavy compression above), and smooths it with a
--   one-pole filter — fast attack, slow release — so the gain trajectory
--   continues across blocks via @gainRef@ (no block-boundary zipper).
compressBuffer :: Ptr CFloat -> Int -> Double -> IORef Double -> IO ()
compressBuffer outPtr totalSamples sr gainRef = do
    g <- readIORef gainRef
    let thr    = fromDBFS (-6.0)
        ratio  = 100.0
        aAtk   = 1.0 - exp (- (1.0 / (0.001 * sr)))    -- 1 ms attack
        aRel   = 1.0 - exp (- (1.0 / (0.325 * sr)))    -- 325 ms release
        go !i !gain
            | i >= totalSamples = writeIORef gainRef gain
            | otherwise = do
                -- stereo-linked detection: max |L| |R|
                xL <- peekElemOff outPtr i
                xR <- peekElemOff outPtr (i + 1)
                let env = max (abs (realToFrac xL)) (abs (realToFrac xR)) :: Double
                    -- >6dBFS above threshold → near-hard limit (100:1)
                    targetGain
                        | env > thr  = (thr + (env - thr) / ratio) / env
                        | otherwise  = 1.0
                    -- attack when reducing gain, release when recovering
                    alpha  = if targetGain < gain then aAtk else aRel
                    gain'  = gain + alpha * (targetGain - gain)
                    yL     = realToFrac (realToFrac xL * gain') :: CFloat
                    yR     = realToFrac (realToFrac xR * gain') :: CFloat
                pokeElemOff outPtr i     yL
                pokeElemOff outPtr (i + 1) yR
                go (i + 2) gain'
    go 0 g

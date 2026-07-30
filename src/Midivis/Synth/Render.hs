{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
{-# LANGUAGE BangPatterns #-}
module Midivis.Synth.Render where

import Prelude
import Control.Monad (forM_, when)
import Data.IORef (IORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, pokeByteOff, peekElemOff, pokeElemOff)
import Foreign.ForeignPtr (withForeignPtr)

import Midivis.Synth.VoicePool
import Midivis.Synth.Types
import Midivis.Util.Math

-- | Render all active voices directly into a PortAudio output buffer.
--   Bulk-reads VoicePool once, renders audio, advances phase per voice.
renderCallback :: VoicePool -> Double -> Int -> Ptr CFloat -> IORef Double -> IO ()
renderCallback vp sr nFrames outPtr _phVar = do
    forM_ [0 .. totalSamples - 1] $ \i ->
        pokeElemOff outPtr i (0.0 :: CFloat)

    let !n = nFrames :: Int
        !bT = fromIntegral nFrames / realToFrac sr :: Float
    let VoicePool fp = vp
    withForeignPtr fp $ \base -> do
        let readSlot i = peek (base `plusPtr` (i * 32)) :: IO VoiceParams
        forM_ [0 .. 127] $ \slot -> do
            VoiceParams freq amp ph env <- readSlot slot
            when (freq /= freqIdleSentinel && env /= Idle) $ do
                let !step    = freq / sr :: Double
                    !ampNorm = amp * defNormalizeAmp

                when (amp /= 0) $
                    forM_ [0 .. n - 1] (\f -> do
                        let !sf = fromIntegral f :: Double
                            !s  = tanh $ realToFrac (sin (2 * pi * (ph + sf * step)) * ampNorm) :: CFloat
                        curL <- peekElemOff outPtr (f * 2)
                        pokeElemOff outPtr (f * 2)     (curL + s)
                        curR <- peekElemOff outPtr (f * 2 + 1)
                        pokeElemOff outPtr (f * 2 + 1) (curR + s))

                let !newPh = ph + fromIntegral n * step
                case env of
                    Attack d c
                        | d <= bT -> do
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  (1.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 defDecay
                        | otherwise -> do
                            let !newD = d - bT
                                !newA = amp + deltaPower amp 1.0 (realToFrac c) (realToFrac (bT / d)) amp
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  newA
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 (Attack newD c)
                    Decay d c
                        | d <= bT -> do
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  defSustainLevel'
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 defSustain
                        | otherwise -> do
                            let !newD = d - bT
                                !newA = amp + deltaPower amp defSustainLevel' (realToFrac c) (realToFrac (bT / d)) amp
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  newA
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 (Decay newD c)
                    Sustain l -> do
                        pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                        pokeByteOff (base `plusPtr` (slot * 32)) 8  (realToFrac l :: Double)
                    Release d c
                        | d <= bT -> do
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 (0.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  (0.0 :: Double)
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 Idle
                        | otherwise -> do
                            let !newD = d - bT
                                !newA = amp + deltaPower amp 0.0 (realToFrac c) (realToFrac (bT / d)) amp
                            pokeByteOff (base `plusPtr` (slot * 32)) 16 newPh
                            pokeByteOff (base `plusPtr` (slot * 32)) 8  newA
                            pokeByteOff (base `plusPtr` (slot * 32)) 24 (Release newD c)
                    Idle -> return ()
  where
    totalSamples = nFrames * 2

{-# LANGUAGE BangPatterns #-}
-- | Three-band parametric EQ: low shelf + peak + high shelf (RBJ cookbook
--   biquads), stereo.  Coefficients are recomputed once per block from the
--   live-modulatable targets; the per-sample path is a plain transposed
--   direct-form-II biquad with zero allocation.
module Midivis.Synth.Eq
    ( EqFx
    , newEqFx
    , setEqTargets
    , processEq
    ) where

import Prelude
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

-- | EQ parameters: (lowGain, midGain, highGain, midFreq, midQ) — gains in dB.
type EqTargets = (Double, Double, Double, Double, Double)

-- | Per-channel biquad delay state: 3 bands × (x1, x2, y1, y2).
data EqChan = EqChan { eqState :: !(IOUArray Int Double) }

-- | The EQ: live-modulatable targets + per-channel state.
data EqFx = EqFx
    { eqTgt :: !(IORef EqTargets)
    , eqL   :: !EqChan
    , eqR   :: !EqChan
    }

-- | Allocate the EQ, all-neutral (0 dB passthrough).
newEqFx :: IO EqFx
newEqFx = do
    t <- newIORef (0, 0, 0, 1000, 1)
    stL <- newArray (0, 11) 0
    stR <- newArray (0, 11) 0
    return $ EqFx t (EqChan stL) (EqChan stR)

-- | Write new targets (modulation evaluator / preset switch).
setEqTargets :: EqFx -> EqTargets -> IO ()
setEqTargets eq t = writeIORef (eqTgt eq) t

-- | RBJ low-shelf coefficient set (second-order, shelf slope 1).
--   Returns (b0, b1, b2, a1, a2) of the normalised difference equation.
shelfCoeffs :: Bool -> Double -> Double -> Double -> (Double, Double, Double, Double, Double)
shelfCoeffs isLow gainDb f0 sr = (b0', b1', b2', a1', a2')
  where
    !a  = 10 ** (gainDb / 40)
    !w0 = 2 * pi * f0 / sr
    !c  = cos w0
    !s  = sin w0
    !b  = if isLow then sqrt (2 * a) else 1 / sqrt (2 * a)
    !bb = 2 * sqrt a * b
    !a0 = (a + 1) + (a - 1) * c + bb * s
    b0' = a * ((a + 1) - (a - 1) * c + bb * s) / a0
    b1' = 2 * a * ((a - 1) - (a + 1) * c) / a0
    b2' = a * ((a + 1) - (a - 1) * c - bb * s) / a0
    a1' = (-2) * ((a - 1) + (a + 1) * c) / a0
    a2' = ((a + 1) + (a - 1) * c - bb * s) / a0

-- | RBJ peaking coefficient set.
peakCoeffs :: Double -> Double -> Double -> Double -> (Double, Double, Double, Double, Double)
peakCoeffs gainDb f0 q sr = (b0', b1', b2', a1', a2')
  where
    !a  = 10 ** (gainDb / 40)
    !w0 = 2 * pi * f0 / sr
    !c  = cos w0
    !s  = sin w0
    !al = s / (2 * q)
    !a0 = 1 + al / a
    b0' = (1 + al * a) / a0
    b1' = (-2 * c) / a0
    b2' = (1 - al * a) / a0
    a1' = (-2 * c) / a0
    a2' = (1 - al / a) / a0

-- | Process a stereo-interleaved CFloat buffer in place through the 3 bands
--   (low shelf → peak → high shelf).  Coefficients are recomputed from the
--   current targets once per block.
processEq :: EqFx -> Double -> Int -> Ptr CFloat -> IO ()
processEq eq sr nFrames outPtr = do
    (lg, mg, hg, mf, mq) <- readIORef (eqTgt eq)
    -- recompute all three band coefficient sets (block-constant)
    let !bandL = shelfCoeffs True  lg 1000 sr
        !bandM = peakCoeffs mg mf mq sr
        !bandH = shelfCoeffs False hg 6000 sr
        cascade :: EqChan -> Int -> Double -> IO Double
        cascade st o x = do
            -- band 1 (low shelf)
            y1 <- biquad st 0 bandL x
            -- band 2 (peak)
            y2 <- biquad st 4 bandM y1
            -- band 3 (high shelf)
            biquad st 8 bandH y2
        biquad :: EqChan -> Int -> (Double, Double, Double, Double, Double) -> Double -> IO Double
        biquad st o (b0, b1, b2, a1, a2) x = do
            x1 <- unsafeRead (eqState st) o
            x2 <- unsafeRead (eqState st) (o + 1)
            y1 <- unsafeRead (eqState st) (o + 2)
            y2 <- unsafeRead (eqState st) (o + 3)
            let !y = b0 * x + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
            unsafeWrite (eqState st) o     x
            unsafeWrite (eqState st) (o + 1) x1
            unsafeWrite (eqState st) (o + 2) y
            unsafeWrite (eqState st) (o + 3) y1
            return y
        runChan st off = go 0
          where
            go !i
                | i >= nFrames = return ()
                | otherwise = do
                    x0 <- peekElemOff outPtr (i * 2 + off) :: IO CFloat
                    y <- cascade st 0 (realToFrac x0)
                    pokeElemOff outPtr (i * 2 + off) (realToFrac y)
                    go (i + 1)
    runChan (eqL eq) 0
    runChan (eqR eq) 1

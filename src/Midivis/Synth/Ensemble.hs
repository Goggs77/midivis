{-# LANGUAGE BangPatterns #-}
-- | Ensemble: wide stereo thickening from a pair of decorrelated delay
--   lines.  The R base delay is offset from L by @width@ samples and the
--   shared slow LFO sweeps the two lines in antiphase, so the image
--   breathes wider without the metallic comb of a single flanger.  Wet mix
--   is fixed at 0.5 (ensemble is a wet effect); targets are (width, rate).
module Midivis.Synth.Ensemble
    ( EnsembleFx
    , newEnsembleFx
    , setEnsembleTargets
    , ensembleBypass
    , processEnsemble
    ) where

import Prelude
import Control.Monad (forM_)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

-- | Ensemble targets: (width samples, rate Hz).
type EnsembleTargets = (Double, Double)

-- | Max delay-line length: 100 ms @48 kHz.
maxEnsembleDelay :: Int
maxEnsembleDelay = 4800

-- | One delay line: circular buffer + write position + LFO phase.
data EnsLine = EnsLine
    { elBuf :: !(IOUArray Int Double)
    , elPos :: !(IORef Int)
    , elPh  :: !(IORef Double)
    }

-- | Full ensemble: targets (smoothed) + L/R lines.
data EnsembleFx = EnsembleFx
    { esTgt :: !(IORef EnsembleTargets)
    , esSm  :: !(IORef EnsembleTargets)
    , esL   :: !EnsLine
    , esR   :: !EnsLine
    }

-- | Allocate (width 0 → passthrough-like behaviour).
newEnsembleFx :: IO EnsembleFx
newEnsembleFx = do
    t <- newIORef (0, 0.3)
    s <- newIORef (0, 0.3)
    l <- newEnsLine
    r <- newEnsLine
    return $ EnsembleFx t s l r
  where
    newEnsLine = do
        buf <- newArray (0, maxEnsembleDelay - 1) 0
        pos <- newIORef 0
        ph  <- newIORef 0
        return $ EnsLine buf pos ph

-- | Write new targets (modulation evaluator / preset switch).
setEnsembleTargets :: EnsembleFx -> EnsembleTargets -> IO ()
setEnsembleTargets es t = writeIORef (esTgt es) t

-- | Bypass when the width is ~0 — no L/R decorrelation is requested.  This
--   also removes the default 800-sample shared delay that width=0 used to
--   keep, so a flat preset now passes through untouched.
ensembleBypass :: EnsembleFx -> IO Bool
ensembleBypass es = do
    (w, _) <- readIORef (esTgt es)
    return (abs w < 1e-9)

-- | Triangular wave in [-1, 1].
tri :: Double -> Double
tri ph = 4 * abs (ph - 0.5) - 1

-- | Read at a fractional position with linear interpolation.
readLine :: EnsLine -> Double -> IO Double
readLine el d = do
    let i0   = floor d
        frac = d - fromIntegral i0
        len  = maxEnsembleDelay
    v0 <- unsafeRead (elBuf el) (i0 `mod` len)
    v1 <- unsafeRead (elBuf el) ((i0 + 1) `mod` len)
    return (v0 + frac * (v1 - v0))

-- | Push one sample; returns the delayed sample.
pushLine :: EnsLine -> Double -> Double -> IO Double
pushLine el delay x = do
    pos <- readIORef (elPos el)
    unsafeWrite (elBuf el) pos x
    let readPos = fromIntegral pos - delay
    out <- readLine el readPos
    let pos' = pos + 1
    writeIORef (elPos el) (if pos' >= maxEnsembleDelay then 0 else pos')
    return out

-- | Advance the line LFO phase.
advancePh :: EnsLine -> Double -> IO ()
advancePh el step = do
    ph <- readIORef (elPh el)
    let !ph' = ph + step
    writeIORef (elPh el) (if ph' >= 1 then ph' - 1 else ph')

-- | Process a stereo buffer in place: L = (dry + wet)/2 etc. with the two
--   lines driven in antiphase by the shared slow LFO.
processEnsemble :: EnsembleFx -> Double -> Int -> Ptr CFloat -> IO ()
processEnsemble es sr nFrames outPtr = do
    (wT, rT) <- readIORef (esTgt es)
    (w, r)   <- readIORef (esSm es)
    let !k = 0.1
        !w' = w + k * (wT - w)
        !r' = r + k * (rT - r)
        step = r' / sr
        baseL = 800.0
        baseR = 800.0 + w'
    writeIORef (esSm es) (w', r')
    forM_ [0 .. nFrames - 1] $ \i -> do
        ph <- readIORef (elPh (esL es))
        let lfo = tri ph
        -- antiphase: L swept up while R swept down
        xL <- peekElemOff outPtr (i * 2) :: IO CFloat
        yL <- pushLine (esL es) (baseL + 200 * (lfo + 1)) (realToFrac xL)
        advancePh (esL es) step
        xR <- peekElemOff outPtr (i * 2 + 1) :: IO CFloat
        yR <- pushLine (esR es) (baseR - 200 * (lfo + 1)) (realToFrac xR)
        advancePh (esR es) step
        let oL = 0.5 * (realToFrac xL + yL)
            oR = 0.5 * (realToFrac xR + yR)
        pokeElemOff outPtr (i * 2)     (realToFrac oL)
        pokeElemOff outPtr (i * 2 + 1) (realToFrac oR)

{-# LANGUAGE BangPatterns #-}
-- | "Trinity" chorus: three modulated delay lines per channel, each with its
--   own triangular-wave LFO sweeping the read position.  Classic detuned
--   shimmer — 3 voices per side × L/R = the "trinity" thickening.  The wet
--   mix is live-modulatable; dry passes through.
module Midivis.Synth.Chorus
    ( ChorusFx
    , newChorusFx
    , setChorusTargets
    , processChorus
    ) where

import Prelude
import Control.Monad (forM, forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

-- | Chorus targets: (mix [0,1], depth samples, rate Hz).
type ChorusTargets = (Double, Double, Double)

-- | Max delay-line length (samples): 50 ms @48 kHz.
maxChorusDelay :: Int
maxChorusDelay = 2400

-- | One delay line: circular buffer + write position + LFO phase.
data DelayLine = DelayLine
    { dlBuf :: !(IOUArray Int Double)
    , dlPos :: !(IORef Int)
    , dlPh  :: !(IORef Double)
    }

-- | One channel: 3 delay lines (the trinity).
data ChorusChan = ChorusChan { ccLines :: [DelayLine] }

-- | Full chorus: live-modulatable targets + per-channel state.
data ChorusFx = ChorusFx
    { chTgt  :: !(IORef ChorusTargets)
    , chSm   :: !(IORef ChorusTargets)   -- smoothed (used per block)
    , chL    :: !ChorusChan
    , chR    :: !ChorusChan
    }

-- | Allocate the chorus (mix 0 → passthrough).
newChorusFx :: IO ChorusFx
newChorusFx = do
    t <- newIORef (0, 0, 0.8)
    s <- newIORef (0, 0, 0.8)
    l <- newChorusChan
    r <- newChorusChan
    return $ ChorusFx t s l r
  where
    newChorusChan = ChorusChan <$> forM [0 .. 2] (\_ -> do
        buf <- newArray (0, maxChorusDelay - 1) 0
        pos <- newIORef 0
        ph  <- newIORef 0
        return $ DelayLine buf pos ph)

-- | Write new targets (modulation evaluator / preset switch).
setChorusTargets :: ChorusFx -> ChorusTargets -> IO ()
setChorusTargets ch t = writeIORef (chTgt ch) t

-- | Triangular wave in [-1, 1] from a phase in [0, 1).
tri :: Double -> Double
tri ph = 4 * abs (ph - 0.5) - 1

-- | Read the delay line at a fractional position with linear interpolation.
readDelay :: DelayLine -> Double -> IO Double
readDelay dl d = do
    let i0  = floor d
        frac = d - fromIntegral i0
        len = maxChorusDelay
    v0 <- unsafeRead (dlBuf dl) (i0 `mod` len)
    v1 <- unsafeRead (dlBuf dl) ((i0 + 1) `mod` len)
    return (v0 + frac * (v1 - v0))

-- | Push one sample through one delay line: write at pos, read back at
--   pos − delay (fractional, interpolated).
pushDelay :: DelayLine -> Double -> Double -> IO Double
pushDelay dl delay x = do
    pos <- readIORef (dlPos dl)
    unsafeWrite (dlBuf dl) pos x
    let readPos = fromIntegral pos - delay
    out <- readDelay dl readPos
    let pos' = pos + 1
    writeIORef (dlPos dl) (if pos' >= maxChorusDelay then 0 else pos')
    return out

-- | Advance a delay line's LFO phase by @step@ (rate/sr).
advanceLfo :: DelayLine -> Double -> IO ()
advanceLfo dl step = do
    ph <- readIORef (dlPh dl)
    let !ph' = ph + step
    writeIORef (dlPh dl) (if ph' >= 1 then ph' - 1 else ph')

-- | Process a stereo-interleaved CFloat buffer in place.  Per sample each
--   channel runs its 3 delay lines with triangle-LFO-modulated delays and
--   mixes them (wet) back into the signal.  Targets are smoothed per block.
processChorus :: ChorusFx -> Double -> Int -> Ptr CFloat -> IO ()
processChorus ch sr nFrames outPtr = do
    (mixT, depthT, rateT) <- readIORef (chTgt ch)
    (mix, depth, rate)    <- readIORef (chSm ch)
    let !k = 0.1
        !mix'   = mix   + k * (mixT - mix)
        !depth' = depth + k * (depthT - depth)
        !rate'  = rate  + k * (rateT - rate)
        step = rate' / sr
    writeIORef (chSm ch) (mix', depth', rate')
    forM_ [0 .. nFrames - 1] $ \i -> do
        -- L channel
        xL <- peekElemOff outPtr (i * 2) :: IO CFloat
        yL <- runChan (chL ch) step depth' (realToFrac xL)
        let oL = (1 - mix') * realToFrac xL + mix' * yL
        pokeElemOff outPtr (i * 2) (realToFrac oL)
        -- R channel
        xR <- peekElemOff outPtr (i * 2 + 1) :: IO CFloat
        yR <- runChan (chR ch) step depth' (realToFrac xR)
        let oR = (1 - mix') * realToFrac xR + mix' * yR
        pokeElemOff outPtr (i * 2 + 1) (realToFrac oR)
  where
    -- three lines, base delays 400/800/1200 samples, LFO phase offsets
    -- 0/0.33/0.66 so the voices never move in unison.
    runChan chan step depth x = do
        let lines = ccLines chan
            bases = [400, 800, 1200]
            phases = [0, 1 / 3, 2 / 3]
            go acc [] [] [] = return acc
            go acc (dl:dlr) (b:br) (p:pr) = do
                ph <- readIORef (dlPh dl)
                let lfo = tri (ph + p)
                    delay = fromIntegral b + max 0 (depth * (lfo + 1) / 2)
                y <- pushDelay dl delay x
                advanceLfo dl step
                go (acc + y) dlr br pr
            go acc _ _ _ = return acc
        go 0 lines bases phases
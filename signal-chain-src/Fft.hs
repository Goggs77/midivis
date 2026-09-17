{-# LANGUAGE BangPatterns #-}
-- | Hand-written radix-2 Cooley-Tukey FFT for real-time convolution reverb.
--   Pure Haskell, zero new dependencies.  All work buffers are pre-allocated
--   in 'FftPlan' at setup time so the PortAudio callback performs no
--   allocation (only IOUArray unsafe reads/writes).
module Midivis.Synth.Fft
    ( FftPlan
    , newFftPlan
    , fftInPlace
    , ifftInPlace
    ) where

import Prelude
import Control.Monad (forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newListArray)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

-- | Pre-allocated FFT workspace.  @fpN@ must be a power of two.
data FftPlan = FftPlan
    { fpN   :: !Int                       -- transform size (power of 2)
    , fpRev :: IOUArray Int Int           -- bit-reversal permutation
    , fpCos :: IOUArray Int Double        -- twiddle cos table, size n/2
    , fpSin :: IOUArray Int Double        -- twiddle sin table, size n/2
    }

-- | Create an FFT plan of size @n@ (power of two).  Allocates all tables.
newFftPlan :: Int -> IO FftPlan
newFftPlan n = do
    let n2 = n `div` 2
    rev <- newListArray (0, n - 1) [bitReverse n i | i <- [0 .. n - 1]]
    cosT <- newListArray (0, n2 - 1) [cos (2 * pi * fromIntegral k / fromIntegral n) | k <- [0 .. n2 - 1]]
    sinT <- newListArray (0, n2 - 1) [sin (2 * pi * fromIntegral k / fromIntegral n) | k <- [0 .. n2 - 1]]
    return $ FftPlan n rev cosT sinT

-- | Reverse the low @bits@ bits of @i@ (bits = log2(n), i.e. n/2 iterations).
bitReverse :: Int -> Int -> Int
bitReverse n i = go 0 i (n `div` 2)
  where
    go !acc !x 0 = acc
    go acc x bits =
        let acc' = (acc `shiftL` 1) .|. (x .&. 1)
        in go acc' (x `shiftR` 1) (bits `shiftR` 1)

-- | In-place forward FFT.  @re@ and @im@ hold the real/imaginary parts of the
--   input (length @n@); on return they hold the spectrum.  No allocation.
--   Iterative radix-2 Cooley-Tukey: bit-reversal permutation first, then
--   log2(n) butterfly stages doubling the transform length each time.
fftInPlace :: FftPlan -> IOUArray Int Double -> IOUArray Int Double -> IO ()
fftInPlace plan re im = do
    let n = fpN plan
    -- 1. bit-reversal permutation (precomputed index table, O(1) per swap)
    forM_ [0 .. n - 1] $ \i -> do
        j <- unsafeRead (fpRev plan) i
        when (i < j) $ do
            ri <- unsafeRead re i
            rj <- unsafeRead re j
            ii <- unsafeRead im i
            ij <- unsafeRead im j
            unsafeWrite re i rj
            unsafeWrite im i ij
            unsafeWrite re j ri
            unsafeWrite im j ii
    -- 2. butterflies: len doubles each stage (2, 4, 8, ... n); twiddles come
    --    from the precomputed cos/sin tables (indexed by j·step, O(1) each).
    let goLen :: Int -> IO ()
        goLen !len
            | len > n    = return ()
            | otherwise  = do
                let half = len `div` 2
                    step = n `div` len
                forM_ [0, len .. n - 1] $ \i -> do
                    forM_ [0 .. half - 1] $ \j -> do
                        let wIdx = j * step
                        wr <- unsafeRead (fpCos plan) wIdx
                        wi <- unsafeRead (fpSin plan) wIdx
                        let a = i + j
                            b = i + j + half
                        ar <- unsafeRead re a
                        ai <- unsafeRead im a
                        br <- unsafeRead re b
                        bi <- unsafeRead im b
                        let !vre = br * wr - bi * wi
                            !vim = br * wi + bi * wr
                        unsafeWrite re a (ar + vre)
                        unsafeWrite im a (ai + vim)
                        unsafeWrite re b (ar - vre)
                        unsafeWrite im b (ai - vim)
                goLen (len * 2)
    goLen 2

-- | In-place inverse FFT (normalised output).
--   IFFT = conjugate → forward FFT → conjugate, scaled by 1/n.
ifftInPlace :: FftPlan -> IOUArray Int Double -> IOUArray Int Double -> IO ()
ifftInPlace plan re im = do
    let n = fpN plan
        inv = 1.0 / fromIntegral n
    -- conjugate
    forM_ [0 .. n - 1] $ \i -> do
        v <- unsafeRead im i
        unsafeWrite im i (-v)
    fftInPlace plan re im
    -- conjugate + scale
    forM_ [0 .. n - 1] $ \i -> do
        r <- unsafeRead re i
        v <- unsafeRead im i
        unsafeWrite re i (r * inv)
        unsafeWrite im i (-v * inv)

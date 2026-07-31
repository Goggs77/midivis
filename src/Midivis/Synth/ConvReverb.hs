{-# LANGUAGE BangPatterns #-}
-- | Real-time convolution reverb (uniform partitioned convolution).
--
--   Signal flow: dry passes through untouched; wet path is
--       input → block buffer (N samples) → FFT(2N) → frequency-domain
--       delay line (P most recent spectra) → Σ X_p ⊙ H_p → IFFT → overlap-add
--   The output is delayed by exactly N samples (the block latency).
--
--   All buffers are pre-allocated at setup; the per-callback path performs
--   zero heap allocation (IOUArray unsafe reads/writes only).
module Midivis.Synth.ConvReverb
    ( ConvReverb
    , newConvReverb
    , syntheticIR
    , processReverb
    ) where

import Prelude
import Control.Monad (forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import qualified Data.Vector.Unboxed as UV
import System.Random (randomRs, mkStdGen)

import Midivis.Synth.Fft
import Midivis.Synth.Types (defConvMaxTail, defSampleRate)

-- | Per-channel convolution state.
data ConvState = ConvState
    { csInRe    :: IOUArray Int Double    -- 2N: input block (real)
    , csInIm    :: IOUArray Int Double    -- 2N: input block (imag, always 0)
    , csFDLRe   :: IOUArray Int Double    -- P×2N: frequency-domain delay line
    , csFDLIm   :: IOUArray Int Double
    , csAccRe   :: IOUArray Int Double    -- 2N: accumulator spectrum
    , csAccIm   :: IOUArray Int Double
    , csOverlap :: IOUArray Int Double    -- N: overlap-add tail
    , csOutBuf  :: IOUArray Int Double    -- N: block output buffer
    ,     csInPos   :: IORef Int              -- samples buffered in current block
    , csFdlPos  :: IORef Int              -- circular write pos in delay line
    , csRdCnt   :: IORef Int              -- remaining samples readable from outBuf
    }

-- | The full reverb: shared IR spectra + one state per channel.
data ConvReverb = ConvReverb
    { crN     :: !Int
    , crFftN  :: !Int
    , crP     :: !Int
    , crPlan  :: FftPlan
    , crIRRe  :: IOUArray Int Double      -- P×2N: IR partition spectra
    , crIRIm  :: IOUArray Int Double
    , crDry   :: !Double                  -- dry gain
    , crWet   :: !Double                  -- wet gain
    , crGain  :: !Double                  -- master output gain
    , crL     :: ConvState
    , crR     :: ConvState
    }

-- | Build the reverb.  @ir@ is the mono impulse response (normalised here),
--   @n@ the partition size (power of two), @dry@/@wet@ the mix, @gain@ the
--   master output level.
newConvReverb :: Int -> [Double] -> Double -> Double -> Double -> IO ConvReverb
newConvReverb n ir0 dry wet gain = do
    -- Truncate the IR tail to keep the multiply-accumulate load bounded.
    let ir      = take (round (defConvMaxTail * defSampleRate)) ir0
        irLen   = length ir
        p       = max 1 ((irLen + n - 1) `div` n)
        fftn    = 2 * n
        -- Total-energy normalisation (Σ h² = 1): keeps the wet output RMS in
        -- the same ballpark as the dry input regardless of IR length.
        energy = sqrt (sum (map (\x -> x * x) ir))
        norm   = max energy 1.0e-9
        irN    = UV.fromList (map (/ norm) ir)
    plan  <- newFftPlan fftn
    irRe  <- newArray (0, p * fftn - 1) 0
    irIm  <- newArray (0, p * fftn - 1) 0
    tmpRe <- newArray (0, fftn - 1) 0
    tmpIm <- newArray (0, fftn - 1) 0
    forM_ [0 .. p - 1] $ \q -> do
        forM_ [0 .. n - 1] $ \k -> do
            let idx = q * n + k
            when (idx < irLen) $ unsafeWrite tmpRe k (irN UV.! idx)
        -- CRITICAL: clear BOTH halves of tmpIm (and tmpRe upper).  tmpIm's
        -- lower half holds the previous partition's spectrum imaginary part;
        -- leaving it would corrupt the next complex FFT and blow up the IR
        -- spectra exponentially across partitions.
        forM_ [0 .. fftn - 1] $ \k -> do
            unsafeWrite tmpRe k 0
            unsafeWrite tmpIm k 0
        forM_ [0 .. n - 1] $ \k -> do
            let idx = q * n + k
            when (idx < irLen) $ unsafeWrite tmpRe k (irN UV.! idx)
        fftInPlace plan tmpRe tmpIm
        forM_ [0 .. fftn - 1] $ \k -> do
            unsafeWrite irRe (q * fftn + k) =<< unsafeRead tmpRe k
            unsafeWrite irIm (q * fftn + k) =<< unsafeRead tmpIm k
    stL <- newConvState n p fftn
    stR <- newConvState n p fftn
    return $ ConvReverb n fftn p plan irRe irIm dry wet gain stL stR

newConvState :: Int -> Int -> Int -> IO ConvState
newConvState n p fftn = do
    inRe <- newArray (0, fftn - 1) 0
    inIm <- newArray (0, fftn - 1) 0
    fdlRe <- newArray (0, p * fftn - 1) 0
    fdlIm <- newArray (0, p * fftn - 1) 0
    accRe <- newArray (0, fftn - 1) 0
    accIm <- newArray (0, fftn - 1) 0
    overlap <- newArray (0, n - 1) 0
    outBuf <- newArray (0, n - 1) 0
    inPos <- newIORef 0
    fdlPos <- newIORef 0
    rdCnt <- newIORef 0
    return $ ConvState inRe inIm fdlRe fdlIm accRe accIm overlap outBuf inPos fdlPos rdCnt

-- | Deterministic decaying-noise impulse response, @len@ samples, T60 in seconds
--   (at 96 kHz).  Useful for testing before a real IR file exists.
syntheticIR :: Int -> Double -> [Double]
syntheticIR len t60 =
    let g = mkStdGen 42
        noises = randomRs (-1.0, 1.0) g
        sr = 96000.0
    in [ x * exp (-3.0 * fromIntegral k / (t60 * sr))
       | (k, x) <- zip [0 .. len - 1] noises ]

-- | Process a stereo-interleaved CFloat buffer in place: wet convolution on
--   both channels, then dry/wet mix scaled by the master gain.
processReverb :: IORef ConvReverb -> Int -> Ptr CFloat -> IO ()
processReverb revRef nFrames outPtr = do
    rev <- readIORef revRef
    let dry = crDry rev
        wet = crWet rev
        g   = crGain rev
        stL = crL rev
        stR = crR rev
    forM_ [0 .. nFrames - 1] $ \i -> do
        xL <- peekElemOff outPtr (i * 2)
        xR <- peekElemOff outPtr (i * 2 + 1)
        yL <- pushSample rev stL (realToFrac xL)
        yR <- pushSample rev stR (realToFrac xR)
        let oL = (dry * realToFrac xL + wet * yL) * g
            oR = (dry * realToFrac xR + wet * yR) * g
        pokeElemOff outPtr (i * 2)     (realToFrac oL)
        pokeElemOff outPtr (i * 2 + 1) (realToFrac oR)

-- | Feed one sample into the wet path; returns the wet output sample
--   (zero during the initial N-sample latency).
pushSample :: ConvReverb -> ConvState -> Double -> IO Double
pushSample rev st x = do
    pos <- readIORef (csInPos st)
    unsafeWrite (csInRe st) pos x
    unsafeWrite (csInIm st) pos 0
    let pos' = pos + 1
    writeIORef (csInPos st) pos'
    when (pos' == crN rev) $ processBlock rev st
    cnt <- readIORef (csRdCnt st)
    if cnt > 0
        then do
            let idx = crN rev - cnt
            y <- unsafeRead (csOutBuf st) idx
            writeIORef (csRdCnt st) (cnt - 1)
            return y
        else return 0

-- | Convolve one full input block (N samples) and stage N output samples.
processBlock :: ConvReverb -> ConvState -> IO ()
processBlock rev st = do
    let n     = crN rev
        fftn  = crFftN rev
        p     = crP rev
        plan  = crPlan rev
        inRe  = csInRe st
        inIm  = csInIm st
        fdlRe = csFDLRe st
        fdlIm = csFDLIm st
        accRe = csAccRe st
        accIm = csAccIm st
    -- zero pad upper half
    forM_ [n .. fftn - 1] $ \k -> do
        unsafeWrite inRe k 0
        unsafeWrite inIm k 0
    -- input spectrum
    fftInPlace plan inRe inIm
    -- write to delay line at fp, accumulate Y = Σ_q X_{fp-q} ⊙ H_q
    fp <- readIORef (csFdlPos st)
    forM_ [0 .. fftn - 1] $ \k -> do
        unsafeWrite fdlRe (fp * fftn + k) =<< unsafeRead inRe k
        unsafeWrite fdlIm (fp * fftn + k) =<< unsafeRead inIm k
    forM_ [0 .. fftn - 1] $ \k -> do
        unsafeWrite accRe k 0
        unsafeWrite accIm k 0
    forM_ [0 .. p - 1] $ \q -> do
        let src   = (fp - q) `mod` p
            dlOff = src * fftn
            irOff = q * fftn
        forM_ [0 .. fftn - 1] $ \k -> do
            xr <- unsafeRead fdlRe (dlOff + k)
            xi <- unsafeRead fdlIm (dlOff + k)
            hr <- unsafeRead (crIRRe rev) (irOff + k)
            hi <- unsafeRead (crIRIm rev) (irOff + k)
            ar <- unsafeRead accRe k
            ai <- unsafeRead accIm k
            unsafeWrite accRe k (ar + xr * hr - xi * hi)
            unsafeWrite accIm k (ai + xr * hi + xi * hr)
    writeIORef (csFdlPos st) ((fp + 1) `mod` p)
    -- time domain
    ifftInPlace plan accRe accIm
    -- overlap-add: out[k] = acc[k] + overlap[k]; overlap[k] = acc[n+k]
    forM_ [0 .. n - 1] $ \k -> do
        ov <- unsafeRead (csOverlap st) k
        a1 <- unsafeRead accRe k
        a2 <- unsafeRead accRe (n + k)
        unsafeWrite (csOutBuf st) k (a1 + ov)
        unsafeWrite (csOverlap st) k a2
    writeIORef (csInPos st) 0
    writeIORef (csRdCnt st) n

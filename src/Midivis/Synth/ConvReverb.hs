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
import Control.Monad (forM, forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import qualified Data.Vector.Unboxed as UV
import System.Random (randomRs, mkStdGen)

import Midivis.Synth.Fft
import Midivis.Synth.Types (defConvMaxTail, defSampleRate, defConvPreDelay, defConvPreDelayR, defConvSpectrumLimit)

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
    , csPreBuf  :: IOUArray Int Double    -- predelay ring (wet path only)
    , csPreLen  :: !Int                   -- predelay ring length
    , csPrePos  :: IORef Int              -- predelay ring write/read pos
    , csInPos   :: IORef Int              -- samples buffered in current block
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
    -- 1. Truncate the IR tail to keep the multiply-accumulate load bounded
    --    (total MAC load = 2·IR·sr/N, so tail length is a direct cost).
    let ir      = take (round (defConvMaxTail * defSampleRate)) ir0
        irLen   = length ir
        p       = max 1 ((irLen + n - 1) `div` n)
        fftn    = 2 * n
    -- 2. Spectrum-limiting before the partition FFTs: clip each bin's
    --    magnitude to defConvSpectrumLimit.  This removes the IR's spectral
    --    peaks (which would otherwise make wet·|H(f)| exceed the dry level
    --    and cancel whole notes at comb notches — the "F#4 sounds weak"
    --    problem).  The tail energy below the limit is preserved.
    irLimited <- limitSpectrum (nextPow2 irLen) ir irLen defConvSpectrumLimit
    let irN = UV.fromList irLimited
    -- 3. Partition pre-computation: split the (time-domain) IR into P chunks
    --    of N samples, zero-pad each to 2N and FFT — those spectra are what
    --    the per-block convolution multiplies against.
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
    -- 4. Per-channel state; L/R pre-delays differ slightly to decorrelate the
    --    dry/wet comb between channels.
    stL <- newConvState n p fftn (max 1 (round (defConvPreDelay  * defSampleRate)))
    stR <- newConvState n p fftn (max 1 (round (defConvPreDelayR * defSampleRate)))
    return $ ConvReverb n fftn p plan irRe irIm dry wet gain stL stR

-- | Smallest power of two >= @n@.
nextPow2 :: Int -> Int
nextPow2 n = go 1
  where
    go !x | x >= n    = x
          | otherwise = go (x * 2)

-- | FFT -> per-bin magnitude limiting -> IFFT of @ir@ (padded to @m@ points).
--   Returns the first @irLen@ samples of the reshaped impulse response.
--   A plain energy normalisation would leave spectral peaks at +12..+20 dB;
--   clipping each bin keeps max|H| bounded so the wet gain can never exceed
--   the dry level.  NOTE: do NOT re-normalise afterwards — that would lift
--   max|H| back up and undo the whole point.
limitSpectrum :: Int -> [Double] -> Int -> Double -> IO [Double]
limitSpectrum m ir irLen limit = do
    let irUV = UV.fromList ir
    -- forward FFT of the zero-padded IR
    planF <- newFftPlan m
    reA <- newArray (0, m - 1) 0
    imA <- newArray (0, m - 1) 0
    forM_ [0 .. irLen - 1] $ \k ->
        unsafeWrite reA k (irUV UV.! k)
    fftInPlace planF reA imA
    -- per-bin hard limit: |H(k)| → min(|H(k)|, limit), phase untouched
    forM_ [0 .. m - 1] $ \k -> do
        r <- unsafeRead reA k
        i <- unsafeRead imA k
        let mag = sqrt (r * r + i * i)
            s   = if mag > limit then limit / mag else 1.0
        unsafeWrite reA k (r * s)
        unsafeWrite imA k (i * s)
    -- back to time domain, take the first irLen samples
    ifftInPlace planF reA imA
    forM [0 .. irLen - 1] $ \k -> unsafeRead reA k

-- | Allocate one channel's DSP buffers (all fixed-size, zeroed; the callback
--   path never allocates again).
newConvState :: Int -> Int -> Int -> Int -> IO ConvState
newConvState n p fftn preDelay = do
    inRe <- newArray (0, fftn - 1) 0
    inIm <- newArray (0, fftn - 1) 0
    fdlRe <- newArray (0, p * fftn - 1) 0
    fdlIm <- newArray (0, p * fftn - 1) 0
    accRe <- newArray (0, fftn - 1) 0
    accIm <- newArray (0, fftn - 1) 0
    overlap <- newArray (0, n - 1) 0
    outBuf <- newArray (0, n - 1) 0
    preBuf <- newArray (0, preDelay - 1) 0
    prePos <- newIORef 0
    inPos <- newIORef 0
    fdlPos <- newIORef 0
    rdCnt <- newIORef 0
    return $ ConvState inRe inIm fdlRe fdlIm accRe accIm overlap outBuf preBuf preDelay prePos inPos fdlPos rdCnt

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
--   both channels, then dry/wet mix scaled by the master gain.  The wet path
--   goes through a short pre-delay ring so the dry transient is never
--   cancelled by the wet tail (and the dry/wet comb is decorrelated per side).
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
        zL <- delaySample stL yL
        zR <- delaySample stR yR
        let oL = (dry * realToFrac xL + wet * zL) * g
            oR = (dry * realToFrac xR + wet * zR) * g
        pokeElemOff outPtr (i * 2)     (realToFrac oL)
        pokeElemOff outPtr (i * 2 + 1) (realToFrac oR)

-- | Circular pre-delay: writes @y@ at the current position and reads back the
--   sample stored one ring-length ago (i.e. delayed by exactly the ring size).
delaySample :: ConvState -> Double -> IO Double
delaySample st y = do
    pos <- readIORef (csPrePos st)
    unsafeWrite (csPreBuf st) pos y
    out <- unsafeRead (csPreBuf st) pos
    let pos' = pos + 1
    writeIORef (csPrePos st) (if pos' >= csPreLen st then 0 else pos')
    return out

-- | Feed one sample into the wet path; returns the wet output sample
--   (zero during the initial N-sample latency).
--   Samples accumulate into the input block; when N are buffered the block is
--   convolved (processBlock) and its output is drained over the next N calls.
pushSample :: ConvReverb -> ConvState -> Double -> IO Double
pushSample rev st x = do
    pos <- readIORef (csInPos st)
    unsafeWrite (csInRe st) pos x
    unsafeWrite (csInIm st) pos 0
    let pos' = pos + 1
    writeIORef (csInPos st) pos'
    when (pos' == crN rev) $ processBlock rev st
    -- read the next staged output sample (N-sample block latency)
    cnt <- readIORef (csRdCnt st)
    if cnt > 0
        then do
            let idx = crN rev - cnt
            y <- unsafeRead (csOutBuf st) idx
            writeIORef (csRdCnt st) (cnt - 1)
            return y
        else return 0

-- | Convolve one full input block (N samples) and stage N output samples.
--   Uniform partitioned convolution: the block's spectrum goes into a
--   frequency-domain delay line holding the P most recent spectra, and the
--   output spectrum is the sum of each delayed input spectrum times the
--   corresponding IR partition spectrum.
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
    -- 1. zero-pad the upper half of the input block (2N transform)
    forM_ [n .. fftn - 1] $ \k -> do
        unsafeWrite inRe k 0
        unsafeWrite inIm k 0
    -- 2. input spectrum
    fftInPlace plan inRe inIm
    -- 3. write the new spectrum into the delay line at the ring position fp
    fp <- readIORef (csFdlPos st)
    forM_ [0 .. fftn - 1] $ \k -> do
        unsafeWrite fdlRe (fp * fftn + k) =<< unsafeRead inRe k
        unsafeWrite fdlIm (fp * fftn + k) =<< unsafeRead inIm k
    -- 4. accumulate Y = Σ_q X_{fp-q} ⊙ H_q over all P partitions
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
    -- 5. back to time domain
    ifftInPlace plan accRe accIm
    -- 6. overlap-add: out[k] = acc[k] + overlap[k]; overlap[k] = acc[n+k]
    forM_ [0 .. n - 1] $ \k -> do
        ov <- unsafeRead (csOverlap st) k
        a1 <- unsafeRead accRe k
        a2 <- unsafeRead accRe (n + k)
        unsafeWrite (csOutBuf st) k (a1 + ov)
        unsafeWrite (csOverlap st) k a2
    writeIORef (csInPos st) 0
    writeIORef (csRdCnt st) n

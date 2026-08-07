{-# LANGUAGE BangPatterns #-}
-- | Algorithmic reverb (Freeverb topology) — a parameter-tweakable space
--   enhancer inserted BEFORE the convolution reverb.
--
--   Per channel: 8 damped comb filters in parallel (summed), then 4 allpass
--   filters in series (diffusion), then a wet-path pre-delay.  The L/R
--   channels use different comb delay tables, which decorrelates the wet
--   signal and gives a natural stereo width (this is Freeverb's classic
--   stereo trick).
--
--   All buffers are pre-allocated; the callback path performs zero heap
--   allocation.  Parameters live in an IORef (arParams) that ANY thread may
--   write — e.g. a future MIDI CC handler — and are smoothed per block
--   (arSmoothed, ~10% per block) so live changes glide instead of zipper.
module Midivis.Synth.AlgorithmicReverb
    ( AlgParams(..)
    , AlgReverb
    , defAlgParams
    , newAlgReverb
    , processAlgReverb
    ) where

import Prelude
import Control.Monad (forM, forM_)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

import Midivis.Synth.Types
    ( defSampleRate
    , defAlgRoomSize, defAlgDamp, defAlgSpread, defAlgPreDelay, defAlgWet, defAlgDry
    )

-- | All tweakable parameters.  Exposed via 'arParams' so external threads
--   (future MIDI CC) can modulate them in real time.
data AlgParams = AlgParams
    { apRoom     :: !Double   -- 0..1: room size → comb feedback (0.7 + 0.28*room)
    , apDamp     :: !Double   -- 0..1: comb low-pass damping (darker tail)
    , apSpread   :: !Double   -- 0..1: allpass diffusion gain
    , apPreDelay :: !Double   -- seconds: wet-path pre-delay
    , apWet      :: !Double   -- wet mix
    , apDry      :: !Double   -- dry mix
    } deriving (Eq, Show)

-- | Per-channel Freeverb state.
data AlgChannel = AlgChannel
    { acCombBuf     :: [IOUArray Int Double]  -- 8 delay lines, one per comb
    , acCombLen     :: [Int]                  -- line lengths (scaled to 96k)
    , acCombPos     :: IORef Int              -- shared ring pointer (mod len each)
    , acDampState   :: IOUArray Int Double    -- one low-pass state per comb
    , acAllpassBuf  :: [IOUArray Int Double]  -- 4 delay lines
    , acAllpassLen  :: [Int]
    , acAllpassPos  :: IORef Int              -- shared ring pointer
    , acPreBuf      :: IOUArray Int Double    -- wet pre-delay ring
    , acPreLen      :: !Int
    , acPrePos      :: IORef Int
    }

-- | The full algorithmic reverb: CC-writable params + one state per channel.
data AlgReverb = AlgReverb
    { arParams   :: IORef AlgParams   -- ★ target params, any thread may write
    , arSmoothed :: IORef AlgParams   -- block-smoothed working copy
    , arL        :: AlgChannel
    , arR        :: AlgChannel
    }

-- | Defaults assembled from the Types.hs constants.
defAlgParams :: AlgParams
defAlgParams = AlgParams defAlgRoomSize defAlgDamp defAlgSpread defAlgPreDelay defAlgWet defAlgDry

-- | Freeverb delay tables (samples @44.1k), scaled to the project rate.
scale44100 :: Int -> Int
scale44100 d = max 16 (round (fromIntegral d * defSampleRate / 44100.0))

combDelaysL :: [Int]
combDelaysL = map scale44100 [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617]

-- R side: each L delay +~50 samples @96k → decorrelated wet signal
combDelaysR :: [Int]
combDelaysR = map ((+ 50) . scale44100) [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617]

allpassDelays :: [Int]
allpassDelays = map scale44100 [556, 441, 341, 225]

-- | Build the reverb; buffers are sized from @params@ and zeroed once.
newAlgReverb :: AlgParams -> IO AlgReverb
newAlgReverb params = do
    pRef <- newIORef params
    sRef <- newIORef params
    let preLen = max 1 (round (apPreDelay params * defSampleRate))
    stL <- newAlgChannel combDelaysL allpassDelays preLen
    stR <- newAlgChannel combDelaysR allpassDelays preLen
    return $ AlgReverb pRef sRef stL stR

newAlgChannel :: [Int] -> [Int] -> Int -> IO AlgChannel
newAlgChannel combLens apLens preLen = do
    combBufs   <- forM combLens $ \len -> newArray (0, len - 1) 0
    combPos    <- newIORef 0
    dampState  <- newArray (0, length combLens - 1) 0
    apBufs     <- forM apLens $ \len -> newArray (0, len - 1) 0
    apPos      <- newIORef 0
    preBuf     <- newArray (0, preLen - 1) 0
    prePos     <- newIORef 0
    return $ AlgChannel combBufs combLens combPos dampState apBufs apLens apPos preBuf preLen prePos

-- | Process a stereo-interleaved CFloat buffer in place: algorithmic wet on
--   both channels, dry/wet mix written back (this becomes the convolution
--   reverb's input).
processAlgReverb :: IORef AlgReverb -> Int -> Ptr CFloat -> IO ()
processAlgReverb revRef nFrames outPtr = do
    rev <- readIORef revRef
    -- per-block parameter smoothing: glide toward the CC-writable target
    tgt <- readIORef (arParams rev)
    cur <- readIORef (arSmoothed rev)
    let p = lerpParams cur tgt 0.1
    writeIORef (arSmoothed rev) p
    let dry    = apDry p
        wet    = apWet p
        room   = apRoom p
        damp   = apDamp p
        spread = apSpread p
        stL    = arL rev
        stR    = arR rev
    forM_ [0 .. nFrames - 1] $ \i -> do
        xL <- peekElemOff outPtr (i * 2)
        xR <- peekElemOff outPtr (i * 2 + 1)
        yL <- processSample stL (realToFrac xL) room damp spread
        yR <- processSample stR (realToFrac xR) room damp spread
        zL <- delaySample stL yL
        zR <- delaySample stR yR
        let oL = dry * realToFrac xL + wet * zL
            oR = dry * realToFrac xR + wet * zR
        pokeElemOff outPtr (i * 2)     (realToFrac oL)
        pokeElemOff outPtr (i * 2 + 1) (realToFrac oR)

-- | One sample through 8 parallel damped combs + 4 series allpasses.
processSample :: AlgChannel -> Double -> Double -> Double -> Double -> IO Double
processSample ch x room damp spread = do
    pos <- readIORef (acCombPos ch)
    let fb = 0.28 * room + 0.7
    -- parallel combs: out = buf[pos]; the damped value feeds back
    let goComb :: Int -> Double -> IO Double
        goComb !k !acc
            | k >= length (acCombBuf ch) = return acc
            | otherwise = do
                let buf = acCombBuf ch !! k
                    len = acCombLen ch !! k
                    idx = pos `mod` len
                v  <- unsafeRead buf idx
                fs <- unsafeRead (acDampState ch) k
                let filtered = v * (1 - damp) + fs * damp
                unsafeWrite (acDampState ch) k filtered
                unsafeWrite buf idx (x + fb * filtered)
                goComb (k + 1) (acc + v)
    combSum <- goComb 0 0
    writeIORef (acCombPos ch) (pos + 1)
    -- series allpasses: y = -g·x + bufout; buf = x + g·bufout
    let goAp :: Int -> Double -> IO Double
        goAp !k !inp
            | k >= length (acAllpassBuf ch) = return inp
            | otherwise = do
                let buf = acAllpassBuf ch !! k
                    len = acAllpassLen ch !! k
                apPos <- readIORef (acAllpassPos ch)
                let idx = apPos `mod` len
                bufout <- unsafeRead buf idx
                unsafeWrite buf idx (inp + spread * bufout)
                writeIORef (acAllpassPos ch) (apPos + 1)
                goAp (k + 1) (-spread * inp + bufout)
    goAp 0 combSum

-- | Circular pre-delay on the wet path.  True ring: read the OLD value at
--   @pos@ (written acPreLen samples ago) FIRST, then overwrite with @y@ —
--   writing before reading collapses the delay to zero.
delaySample :: AlgChannel -> Double -> IO Double
delaySample ch y = do
    pos <- readIORef (acPrePos ch)
    out <- unsafeRead (acPreBuf ch) pos
    unsafeWrite (acPreBuf ch) pos y
    let pos' = pos + 1
    writeIORef (acPrePos ch) (if pos' >= acPreLen ch then 0 else pos')
    return out

-- | Move every field of @cur@ a fraction @k@ of the way toward @tgt@.
lerpParams :: AlgParams -> AlgParams -> Double -> AlgParams
lerpParams cur tgt k = AlgParams
    { apRoom     = apRoom cur     + k * (apRoom tgt     - apRoom cur)
    , apDamp     = apDamp cur     + k * (apDamp tgt     - apDamp cur)
    , apSpread   = apSpread cur   + k * (apSpread tgt   - apSpread cur)
    , apPreDelay = apPreDelay cur + k * (apPreDelay tgt - apPreDelay cur)
    , apWet      = apWet cur      + k * (apWet tgt      - apWet cur)
    , apDry      = apDry cur      + k * (apDry tgt      - apDry cur)
    }

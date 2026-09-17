{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Midivis.World
import Midivis.System.ExampleRenderer 
import Midivis.Synth.Engine
import Midivis.Synth.Types
import Midivis.Synth.Render (renderCallback, pushVoiceToFx, newVFilterState, newVFilterFresh, SynthFx(..))
import Midivis.Synth.ConvReverb (newConvReverb)
import Midivis.Synth.AlgorithmicReverb (newAlgReverb)
import Midivis.Synth.Eq (newEqFx)
import Midivis.Synth.Chorus (newChorusFx)
import Midivis.Synth.Ensemble (newEnsembleFx)
import Midivis.Synth.Preset
import Midivis.Util.Math (fromDBFS)
import Midivis.Resources.Audio.IR
import Sound.PortAudio
import qualified Sound.PortAudio.Base as Base
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Array.IO (newArray)
import System.IO (hPutStrLn, stderr)
import Foreign.C.Types (CFloat)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (poke, pokeElemOff, sizeOf)
import Foreign.Ptr (plusPtr, Ptr)
import Control.Monad (forM_, forM, forever, when)
import Data.List (maximumBy)
import Data.Function (on)
import Data.Maybe (catMaybes)
import GHC.Conc
import GHC.IO.Encoding

main :: IO ()
main = do
    setLocaleEncoding utf8
    -- Use all available cores (equivalent to +RTS -N)
    cores <- getNumCapabilities
    setNumCapabilities $ min 4 cores
    -- === 1. Create shared state ===
    midiTQue <- newTQueueIO
    -- initWorld now creates the VoicePool and CtrlBus itself; the same
    -- references live in the World (engine thread reads them back, and the
    -- audio callback captures them below — the real-time thread never
    -- touches the TVar).
    w0TVar <- newTVarIO =<< initWorld midiTQue
    w0s <- readWorld w0TVar
    let vp  = voicePool w0s
        bus = ctrlBus w0s
        -- the global sample counter lives in the World: the callback
        -- advances it, the engine reads it to stamp note onsets
        smpRef = sampleCounter w0s
    let fpb = 1024
        nch = 2
        sr  = defSampleRate
    ir' <- ir
    -- Active voice (from the World; preset switching updates it live).
    -- It carries its own waveform, envelope, LFOs, FX and reverb mix.
    let activeV = activeVoice w0s
        rv      = vReverb activeV
    -- [DIAG] log IR stats
    hPutStrLn stderr $ "[Reverb] IR samples=" ++ show (length ir') ++
        " peak=" ++ show (maximum (0 : map abs ir')) ++
        " sum=" ++ show (sum ir')
    -- === 1b. Convolution reverb: load IR file embed ===
    reverbObj <- newConvReverb defConvBlockSize ir' (rvConvDry rv) (rvConvWet rv) (rvConvGain rv)
    reverbRef <- newIORef reverbObj
    -- === 1c. Algorithmic reverb (Freeverb pre-reverb) + stereo widener ===
    algRef <- newIORef =<< newAlgReverb (rvAlg rv)
    -- stereo widener: (aSm, x1, y1) state + modulatable target a
    widthRef <- newIORef (0.0, 0.0, 0.0 :: Double)
    widthTgt <- newIORef defStereoWidth
    -- DC blocker state (prev input/output per channel) + modulatable R
    dcRef <- newIORef (0.0, 0.0, 0.0, 0.0 :: Double)
    dcRTgt <- newIORef 0.9995
    dcRSm  <- newIORef 0.9995
    -- Global sample counter (in the World): drives the random LFOs and note
    -- onset stamps.  (Callback advances it via sfSmpRef = sampleCounter w0s.)
    -- Per-voice smoothed amplitude (click-free aftertouch, 2nd order)
    ampSmoothRef <- newIORef =<< newArray (0, 127) (0.0 :: Double)
    ampMidRef    <- newIORef =<< newArray (0, 127) (0.0 :: Double)
    -- Master compressor parameters (thr, ratio, atkSec, relSec): modulatable
    -- targets + smoothed copy (start at the current hardcoded values).
    let compThr   = fromDBFS (-6.0)
        compRatio = 100.0
        compAtk   = 0.001
        compRel   = 0.325
    compTgt <- newIORef (compThr, compRatio, compAtk, compRel)
    compSm  <- newIORef (compThr, compRatio, compAtk, compRel)
    -- Insert effects: 12-band EQ, trinity chorus, ensemble, per-voice filters
    eqFxRef      <- newIORef =<< newEqFx
    chorusFxRef  <- newIORef =<< newChorusFx
    ensembleFxRef <- newIORef =<< newEnsembleFx
    vFilterRef   <- newIORef =<< newVFilterState
    vFreshRef    <- newIORef =<< newVFilterFresh

    -- === 2. Start synth engine (forkIO mainLoop) ===
    initSynth w0TVar
    hPutStrLn stderr "[Main] Synth engine started"

    -- === 3. Start PortAudio output (forkIO thread, callback mode) ===
    _ <- forkIO $ do
        hPutStrLn stderr "[Audio] Initializing PortAudio..."
        r <- withPortAudio $ do
            -- probe devices 0..4: open each, blast a 440Hz test tone, keep the
            -- first one that produces no error (device 0 is often a phantom).
            let probe devIdx = do
                    let outParams = Just (StreamParameters (fromIntegral devIdx) (fromIntegral nch) (Base.PaTime 0.1))
                    result <- withStream
                        Nothing
                        outParams
                        sr
                        (Just fpb)
                        [ClipOff]
                        Nothing
                        Nothing
                        $ \(strm :: Stream CFloat CFloat) -> do
                            merr <- startStream strm
                            case merr of
                                Just err -> pure $ Left err
                                Nothing -> do
                                    allocaArray (fpb * fromIntegral nch) $ \bufPtr -> do
                                        bufFP <- newForeignPtr_ bufPtr
                                        withForeignPtr bufFP $ \ptr ->
                                            forM_ [0 .. fpb - 1] $ \i -> do
                                                let s = sin (2 * pi * fromIntegral i * 440.0 / realToFrac sr) * 0.5 :: CFloat
                                                    offL = i * 2
                                                    offR = i * 2 + 1
                                                poke (ptr `plusPtr` (offL * sizeOf (undefined :: CFloat))) s
                                                poke (ptr `plusPtr` (offR * sizeOf (undefined :: CFloat))) s
                                        merr2 <- writeStream strm (fromIntegral fpb) bufFP
                                        _ <- stopStream strm
                                        pure $ case merr2 of
                                            Nothing -> Right (fromIntegral devIdx)
                                            Just e  -> Left e
                    case result of
                        Right dev -> pure (Right dev)
                        Left _ | devIdx < 4 -> probe (devIdx + 1)
                        Left err -> pure $ Left err
            devResult <- probe 0
            case devResult of
                Left err -> do
                    hPutStrLn stderr $ "[Audio] No working device: " ++ show err
                    pure $ Left err
                Right devIdx -> do
                    hPutStrLn stderr $ "[Audio] Using device " ++ show devIdx
                    let outParams = Just (StreamParameters (fromIntegral devIdx) (fromIntegral nch) (Base.PaTime 0.1))
                    -- Compressor gain state, carried across callbacks.
                    phVar <- newIORef (1.0 :: Double)
                    -- Active preset, swappable live by the preset switch.
                    voiceRef <- newIORef activeV
                    -- Output block RMS (L, R), written by the callback each
                    -- block; the Gloss thread smooths it into the World.
                    rmsRef <- newIORef (0.0, 0.0 :: Double)
                    -- Last output sample (L, R) in [-1,1], raw, per block.
                    sampleRef <- newIORef (0.0, 0.0 :: Double)
                    -- Bundle every mutable effect/state ref for the callback.
                    let fx = SynthFx
                            { sfGainRef      = phVar
                            , sfCompTgt      = compTgt
                            , sfCompSm       = compSm
                            , sfReverbRef    = reverbRef
                            , sfAlgRef       = algRef
                            , sfSmpRef       = smpRef
                            , sfAmpSmoothRef = ampSmoothRef
                            , sfAmpMidRef    = ampMidRef
                            , sfWidthRef     = widthRef
                            , sfWidthTgt     = widthTgt
                            , sfDcRef        = dcRef
                            , sfDcRTgt       = dcRTgt
                            , sfDcRSm        = dcRSm
                            , sfEqRef        = eqFxRef
                            , sfChorusRef    = chorusFxRef
                            , sfEnsembleRef  = ensembleFxRef
                            , sfVFilterRef   = vFilterRef
                            , sfVFreshRef    = vFreshRef
                            , sfVoice        = voiceRef
                            , sfRmsRef       = rmsRef
                            , sfSampleRef    = sampleRef
                            }
                    -- Push the active voice's FX/reverb base parameters into
                    -- the effect targets (startup + preset switch).
                    pushVoiceToFx activeV fx
                    -- Publish the bundle so the preset switch (UI thread) can
                    -- push new parameters in.
                    writeIORef (synthFxRef w0s) (Just fx)

                    -- The real-time callback: render one frame (fpb = 2048,
                    -- ~21 ms of audio at 96 kHz) and hand it to PortAudio.
                    -- fpb is the only knob that trades latency vs. tolerance
                    -- for the reverb's processBlock burst + system jitter.
                    --
                    -- [DIAG] The callback is wrapped in 'try': a Haskell
                    -- exception escaping through the C FFI boundary kills the
                    -- PortAudio callback thread (permanent silence with the
                    -- stream still nominally "active").  Catching it here
                    -- keeps the stream alive, prints the exception, and lets
                    -- us see exactly what broke.  A heartbeat counter logs
                    -- every 2000th callback so we can tell a dead callback
                    -- thread from a merely silent one.
                    cbCount <- newIORef (0 :: Int)
                    let callback :: StreamCallback CFloat CFloat
                        callback _time _flags nFrames _inp outPtr = do
                            n <- readIORef cbCount
                            writeIORef cbCount (n + 1)
                            when (n `mod` 2000 == 0) $
                                hPutStrLn stderr $ "[Audio] callback alive, frame="
                                    ++ show n
                            r <- try (renderCallback vp sr (fromIntegral nFrames) outPtr fx bus)
                            case r of
                                Left (e :: SomeException) -> do
                                    hPutStrLn stderr $ "[Audio] callback EXCEPTION: " ++ show e
                                    -- silence this frame instead of crashing the stream
                                    forM_ [0 .. fromIntegral nFrames * 2 - 1] $ \i ->
                                        pokeElemOff outPtr i (0.0 :: CFloat)
                                Right () -> return ()
                            return Continue

                    withStream
                        Nothing
                        outParams
                        sr
                        (Just fpb)
                        [ClipOff]
                        (Just callback)
                        Nothing
                        $ \(strm :: Stream CFloat CFloat) -> do
                            serr <- startStream strm
                            case serr of
                                Just e -> do
                                    hPutStrLn stderr $ "[Audio] startStream error: " ++ show e
                                    return $ Left e
                                Nothing -> do
                                    hPutStrLn stderr "[Audio] PortAudio stream active (callback)"
                                    -- [DIAG] Watchdog: print the callback counter
                                    -- every 2 s.  delta==0 ⇒ the callback thread
                                    -- died (silent stream); delta keeps growing
                                    -- but no sound ⇒ the stream itself stopped.
                                    _ <- forkIO $ monitorLoop cbCount
                                    forever $ threadDelay maxBound
        case r of
            Left err -> hPutStrLn stderr $ "[Audio] PortAudio error: " ++ show err
            Right _  -> hPutStrLn stderr "[Audio] PortAudio finished"

    -- Give threads time to initialise
    threadDelay 100000

    -- === 4. Start Gloss visualization (blocks main thread) ===
    hPutStrLn stderr "[Main] Starting Gloss visualization..."
    drawExampleRelative w0TVar

    hPutStrLn stderr "[Main] Shutdown"

-- | [DIAG] Watchdog thread: reports the audio callback counter every 2 s.
monitorLoop :: IORef Int -> IO ()
monitorLoop cnt = go 0
  where
    go prev = do
        threadDelay 2000000
        now <- readIORef cnt
        hPutStrLn stderr $ "[Audio] MONITOR: callbacks=" ++ show now
            ++ " delta=" ++ show (now - prev)
        go now

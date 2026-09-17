{-# LANGUAGE BangPatterns #-}
-- | Full-chain voicing test.
--
--   Drives every preset voice through the complete signal chain
--       voice → EQ → chorus → widen → ensemble → DC block
--             → alg reverb → conv reverb → compressor
--   and logs per-stage level monitoring (per-channel RMS + peak, dBFS)
--   plus chain-stage state to a .log file.
--
--   Offline (no audio device): one A4 note is triggered directly into the
--   VoicePool and ~2.1 s of audio is rendered block by block; every 10th
--   block the levels at each stage boundary are sampled and printed.
--
--   Run: cabal test fullchain-test  (writes fullchain.log in the cwd)
module Main where

import Prelude
import Control.Monad (forM_, forM, when)
import Data.Array.IO (newArray)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff)
import System.IO
import System.Directory (getCurrentDirectory)
import Text.Printf (printf)

import Midivis.Synth.Preset (allPresets, Voice(..), ReverbParams(..), FxParams(..), EnvParams(..), EqBand(..))
import Midivis.Synth.Types
import Midivis.Synth.Render
import Midivis.Synth.VoicePool
import Midivis.Synth.CtrlBus (CtrlBus, newCtrlBus)
import Midivis.Synth.Eq (newEqFx, processEq)
import Midivis.Synth.Chorus (newChorusFx, processChorus)
import Midivis.Synth.Ensemble (newEnsembleFx, processEnsemble)
import Midivis.Synth.AlgorithmicReverb (newAlgReverb, processAlgReverb, AlgParams(..))
import Midivis.Synth.ConvReverb (newConvReverb, processReverb)
import Midivis.Resources.Audio.IR (ir)
import Midivis.Util.Math (fromDBFS)

-- | Block length (frames), blocks to render (~2.13 s @48k), sample rate.
fpb, nBlocks :: Int
fpb = 512
nBlocks = 200

sr :: Double
sr = defSampleRate

-- | The test note: A4 = 440 Hz, slot 69.
noteId :: Int
noteId = 69

noteFreq :: Double
noteFreq = 440.0

data Levels = Levels { lvRmsL :: !Double, lvRmsR :: !Double, lvPeak :: !Double }

-- | Per-channel RMS + overall peak over an interleaved CFloat buffer
--   (total = frames × 2).
sampleLevels :: Ptr CFloat -> Int -> IO Levels
sampleLevels p total = go 0 0 0 0 0
  where
    n = fromIntegral (total `div` 2) :: Double
    go !i !sl !sr' !pl !pr
        | i >= total = return (Levels (sqrt (sl / n)) (sqrt (sr' / n)) (max pl pr))
        | otherwise = do
            xL <- peekElemOff p i
            xR <- peekElemOff p (i + 1)
            let xl = realToFrac xL :: Double
                xr = realToFrac xR :: Double
            go (i + 2) (sl + xl * xl) (sr' + xr * xr)
                (max pl (abs xl)) (max pr (abs xr))

-- | dBFS of a linear amplitude ("-inf" at 0).
db :: Double -> String
db v = if v <= 1.0e-12 then "-inf" else printf "%6.1f" (20 * logBase 10 v)

-- | Build the whole FX bundle exactly like app/Main.hs does.
mkSynthFx :: IO SynthFx
mkSynthFx = do
    ir' <- ir
    let rv0 = vReverb (head allPresets)
    reverbObj <- newConvReverb defConvBlockSize ir' (rvConvDry rv0) (rvConvWet rv0) (rvConvGain rv0)
    reverbRef <- newIORef reverbObj
    algRef    <- newIORef =<< newAlgReverb (rvAlg rv0)
    widthRef  <- newIORef (0.0, 0.0, 0.0 :: Double)
    widthTgt  <- newIORef defStereoWidth
    dcRef     <- newIORef (0.0, 0.0, 0.0, 0.0 :: Double)
    dcRTgt    <- newIORef 0.9995
    dcRSm     <- newIORef 0.9995
    smpRef    <- newIORef (0.0 :: Double)
    ampSmoothRef <- newIORef =<< newArray (0, 127) (0.0 :: Double)
    ampMidRef    <- newIORef =<< newArray (0, 127) (0.0 :: Double)
    let compThr = fromDBFS (-6.0)
    compTgt <- newIORef (compThr, 100.0, 0.001, 0.325)
    compSm  <- newIORef (compThr, 100.0, 0.001, 0.325)
    eqFxRef       <- newIORef =<< newEqFx
    chorusFxRef   <- newIORef =<< newChorusFx
    ensembleFxRef <- newIORef =<< newEnsembleFx
    voiceRef      <- newIORef (head allPresets)
    phVar         <- newIORef (1.0 :: Double)
    return $ SynthFx
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
        , sfVoice        = voiceRef
        }

-- | Render one block through the full chain, sampling levels at every stage
--   boundary.
renderBlockWithLevels :: VoicePool -> CtrlBus -> SynthFx -> Voice -> Ptr CFloat -> IO [(String, Levels)]
renderBlockWithLevels vp bus fx voice outPtr = do
    renderVoices vp sr fpb outPtr fx bus voice
    l0 <- sampleLevels outPtr (fpb * 2)
    eqFx <- readIORef (sfEqRef fx)
    processEq eqFx sr fpb outPtr
    l1 <- sampleLevels outPtr (fpb * 2)
    chFx <- readIORef (sfChorusRef fx)
    processChorus chFx sr fpb outPtr
    l2 <- sampleLevels outPtr (fpb * 2)
    widenR (sfWidthRef fx) (sfWidthTgt fx) (fpb * 2) outPtr
    l3 <- sampleLevels outPtr (fpb * 2)
    esFx <- readIORef (sfEnsembleRef fx)
    processEnsemble esFx sr fpb outPtr
    l4 <- sampleLevels outPtr (fpb * 2)
    dcBlockBuffer (sfDcRef fx) (sfDcRTgt fx) (sfDcRSm fx) (fpb * 2) outPtr
    l5 <- sampleLevels outPtr (fpb * 2)
    processAlgReverb (sfAlgRef fx) fpb outPtr
    l6 <- sampleLevels outPtr (fpb * 2)
    processReverb (sfReverbRef fx) fpb outPtr
    l7 <- sampleLevels outPtr (fpb * 2)
    compressBuffer outPtr (fpb * 2) sr (sfGainRef fx) (sfCompTgt fx) (sfCompSm fx)
    l8 <- sampleLevels outPtr (fpb * 2)
    return [ ("voice", l0), ("EQ", l1), ("chorus", l2), ("widen", l3)
           , ("ensemble", l4), ("dc", l5), ("algRvb", l6), ("convRvb", l7)
           , ("comp", l8) ]

stageHeader :: String
stageHeader = "  block | voice L/R pk | EQ L/R pk | chorus L/R pk | widen L/R pk | ensemble L/R pk | dc L/R pk | algRvb L/R pk | convRvb L/R pk | comp L/R pk"

printBlockRow :: Handle -> Int -> [(String, Levels)] -> IO ()
printBlockRow h b stages = do
    hPutStr h (printf "  %4d " b)
    forM_ stages $ \(nm, lv) -> do
        hPutStr h (printf "| %s/%s %s " (db (lvRmsL lv)) (db (lvRmsR lv)) (db (lvPeak lv)))
    hPutStrLn h ""

presetSummary :: Voice -> String
presetSummary voice =
    "  params: env(a=" ++ fmtF (epAttack e) ++ " d=" ++ fmtF (epDecay e)
    ++ " s=" ++ fmtF (epSustainLevel e) ++ " r=" ++ fmtF (epRelease e)
    ++ ") eq(bands=" ++ show (map ebGain (fxEqBands fx))
    ++ ") chorus(mix=" ++ fmtD (fxChorusMix fx) ++ " depth=" ++ fmtD (fxChorusDepth fx)
    ++ ") ens(w=" ++ fmtD (fxEnsembleWidth fx)
    ++ ") vib(d=" ++ fmtD (fxVibDepth fx) ++ " r=" ++ fmtD (fxVibRate fx)
    ++ ") conv(dry=" ++ fmtD (rvConvDry rv) ++ " wet=" ++ fmtD (rvConvWet rv) ++ " gain=" ++ fmtD (rvConvGain rv)
    ++ ") alg(wet=" ++ fmtD (apWet (rvAlg rv)) ++ " room=" ++ fmtD (apRoom (rvAlg rv))
    ++ ") matrix=" ++ show (length (vMatrix voice)) ++ " rows"
  where
    e  = vEnv voice
    fx = vFx voice
    rv = vReverb voice
    fmtF :: Float -> String
    fmtF x = printf "%.2f" x
    fmtD :: Double -> String
    fmtD x = printf "%.2f" x

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    cwd <- getCurrentDirectory
    let logPath = "fullchain.log"
    logH <- openFile logPath WriteMode
    hSetBuffering logH LineBuffering
    hPutStrLn logH "midivis full-chain voicing test"
    hPutStrLn logH (printf "sampleRate=%d fpb=%d blocks=%d note=slot %d @ %.1f Hz" (round sr :: Int) fpb nBlocks noteId noteFreq)
    hPutStrLn logH "signal chain: voice -> EQ -> chorus -> widen -> ensemble -> dc -> algReverb -> convReverb -> compressor"
    hPutStrLn logH "levels: L-RMS/R-RMS (dBFS) and overall peak (dBFS) sampled at each stage boundary"
    hPutStrLn logH ""

    vp  <- newVoicePool
    bus <- newCtrlBus

    forM_ (zip [0 :: Int ..] allPresets) $ \(idx, voice) -> do
        -- fresh FX chain per preset: delay lines / reverbs must not carry
        -- the previous preset's tail into this measurement
        fx <- mkSynthFx
        -- load the preset's FX/reverb parameters into the live targets
        pushVoiceToFx voice fx
        writeIORef (sfVoice fx) voice
        -- trigger one A4: frequency, onset stamp, and all three envelopes
        writeFreq  vp noteId noteFreq
        writeOnset vp noteId 0
        writePhase vp noteId 0
        writeAmp   vp noteId 0
        let pE  = vEnv voice
            pE2 = vEnv2 voice
            pE3 = vEnv3 voice
        writeEnv  vp noteId (Attack (epAttack pE)  (epAttackCurve pE))
        writeEnv2 vp noteId (Attack (epAttack pE2) (epAttackCurve pE2))
        writeEnv3 vp noteId (Attack (epAttack pE3) (epAttackCurve pE3))

        hPutStrLn logH ""
        hPutStrLn logH ("=== preset [" ++ show idx ++ "] ===")
        hPutStrLn logH (presetSummary voice)
        hPutStrLn logH stageHeader

        allocaArray (fpb * 2) $ \outPtr -> do
            forM_ [1 .. nBlocks] $ \b -> do
                stages <- renderBlockWithLevels vp bus fx voice outPtr
                when (b == 1 || b `mod` 10 == 0 || b == nBlocks) $
                    printBlockRow logH b stages

        -- stage state after the run
        env1 <- readEnv vp noteId
        env2 <- readEnv2 vp noteId
        env3 <- readEnv3 vp noteId
        a2   <- readEnv2Amp vp noteId
        a3   <- readEnv3Amp vp noteId
        hPutStrLn logH $ "  final state: env1=" ++ show env1
            ++ " env2=" ++ show env2 ++ " env2amp=" ++ printf "%.3f" a2
            ++ " env3=" ++ show env3 ++ " env3amp=" ++ printf "%.3f" a3

    hClose logH
    putStrLn $ "full-chain test written to " ++ cwd ++ "\\" ++ logPath

{-# LANGUAGE OverloadedStrings #-}
module Midivis.System.ExampleRenderer where
import Midivis.Render.Example
import Midivis.EventHandler
import Midivis.System.MidiQuery
import Midivis.Midi.MidiParser (MidiEvent)
import Midivis.World 
import Midivis.Util.Math
import Midivis.Synth.Render (pushVoiceToFx, SynthFx(..))
import Midivis.Resources.Tuning.All (allTunings)
import Midivis.Synth.Preset (allPresets)
import Midivis.Tuning.Poly
import Midivis.Tuning.Corpus

import Graphics.Gloss.Relative
import Graphics.Gloss.Interface.Environment
import Sound.RtMidi (InputDevice)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.StorableVector as SV
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Midivis.Tuning.Ascl (calibrateA4, hasReferencePitch)
import Midivis.Resources.Tuning.All
import Midivis.Synth.VoicePool (readVoice)
import Data.Maybe (mapMaybe)
import Midivis.Synth.Types (VoiceParams(..), EnvelopeState (..), isReleasingOrIdle, isIdle)
import Control.Monad (when)
import Midivis.Util.Text
import Midivis.Synth.CtrlBus





drawExampleRelative :: TVar World -> IO ()
drawExampleRelative w0TVar = do
    -- MIDI polling moved off the step chain: a standalone thread reads the
    -- device at ~1 kHz and feeds a TQueue; the GUI thread drains it each
    -- tick (drainMidi) so midiEvtBuf stays single-threaded.  This lets the
    -- simulation loop run at render rate (288) without losing MIDI latency.
    pollQue <- newTQueueIO
    inputDevice <- initMidi
    _ <- forkIO $ midiPollThread inputDevice pollQue
    w0 <- readWorld w0TVar
    (_, h) <- getScreenSize
    -- 1151 and 809 are primes for no special reason, keep it that way
    let size = (round (1151.0 * ratio), round (809.0 * ratio)) where ratio = fromIntegral h/1080.0
    case inputDevice of
        Nothing -> putStrLn "[Debug] No MIDI keyboard — computer keyboard active"
        Just _  -> putStrLn "[Debug] Midi Initialized"
    playRelativeIO
        (InWindow "Midivis" size (300,100))
        black
        288 -- should be at least 2*fixed frame rate
        w0
        drawRelativeFrame
        handleEvent
        (step w0TVar pollQue)

-- | Time management and modulation update
stepTime :: Float -> World -> IO World
stepTime dT w0 = 
    return w0{
        time = time w0 + dT, 
        totalFrames = totalFrames w0 + 1,
        initialized = True,
        fixedFrameCounter = if fixedFrameCounter w0 >= 1.0 / fromIntegral (fixedFrameRate w0) 
            then 0 
            else fixedFrameCounter w0 + dT
        }

stepAngle :: World -> IO World
stepAngle w0 = do
    pb <- specialNorm (ctrlBus w0) pitchBendSlot
    let pb' = pb / 144.0
    if isFixedFrame w0 then return w0{
    bufferVelocity = lerp (bufferVelocity w0) (pb' + clockVelocity w0 /(0.3+1.5*(fromIntegral (midiLength w0) - 0.1) )) 0.02,
    baseAngle = baseAngle w0 + bufferVelocity w0,
    bufferTransparency = lerp (bufferTransparency w0) (fromIntegral (SV.length $ midiEvtCpy w0) / 77.0) 0.01
    } 
    else return w0

stepTuning :: World -> IO World
stepTuning w0 = if totalFrames w0 `rem` 90 == 0 && tuningScroll w0 /= Stop
    then do 
        let i = case (tuningScroll w0) of
                Next -> 1
                Stop -> 0
                Prev -> (-1)
            newIndex = (sclIndex w0 + (length allTunings + i)) `rem` (length allTunings)
            tun = allTunings !! newIndex
            -- Prefer the file's own @ABL REFERENCE_PITCH (parseAscl already
            -- applied it at compile time); fall back to A4 = freqA4 when the
            -- file declares none.
            chosen = if hasReferencePitch tun
                        then tun
                        else calibrateA4 tun (freqA4 w0)
        return w0 {
        sclIndex = newIndex,
        sclOfChoice = chosen
        }
    else return w0

-- | Preset switching (',' / '.'): scroll presetIndex, publish the new voice
--   to the audio callback (sfVoice) and push its FX/reverb parameters into
--   the live effect targets (glides, not clicks).
stepPreset :: World -> IO World
stepPreset w0 = if totalFrames w0 `rem` 90 == 0 && presetScroll w0 /= Stop
    then do
        let i = case (presetScroll w0) of
                Next -> 1
                Stop -> 0
                Prev -> (-1)
            newIndex = (presetIndex w0 + (length allPresets + i)) `rem` (length allPresets)
            newVoice = allPresets !! newIndex
        mfx <- readIORef (synthFxRef w0)
        case mfx of
            Just fx -> do
                writeIORef (sfVoice fx) newVoice
                pushVoiceToFx newVoice fx
            Nothing -> return ()
        return w0 {
        presetIndex = newIndex,
        activeVoice = newVoice
        }
    else return w0

-- | This is the only function where gui thread reads 'VoicePool'
stepChord :: World -> IO World
stepChord w0 = do
    if (not $ initialized w0) then clearName 0 else do
        voices <- V.mapM (readVoice $ voicePool w0) $ V.fromList [0..127]
        let freqs = (flip V.mapMaybe) voices (\ vp -> 
                if (vpAmplitude vp > 0.008 && (not . isReleasingOrIdle) vp) 
                    then (Just $ vpFrequency vp) 
                    else Nothing)
        let amps = V.foldl' (+) 0 $ (flip V.mapMaybe) voices (\ vp -> 
                if  (not . isIdle) vp
                    then (Just $ vpAmplitude vp) 
                    else Nothing)
        if (not $ V.length freqs >= 2) then clearName amps else do
            let maybe' = fitAll $ fromFreqs freqs
            case maybe' of
                Just (t, d, i) -> updateName (t, d, i) amps
                Nothing -> clearName amps
    where 
        updateName (t, d, i) x = if d >= 1 then clearName x else case (polyName t) of
            Just name -> do
                return w0 {chordName = T.unwords [name, T.concat ["△ ≐ ", floatShow d], T.concat ["⇥ ", T.show i]], ampsum = x}
            Nothing -> clearName x
        clearName x = return w0 {chordName = T.pack "", ampsum = x}

-- | Sync the audio callback's output meters into UI-owned World fields:
--   smooths the raw block RMS (SynthFx.sfRmsRef) into rmsSmL/rmsSmR
--   (attack fast, release slow — VU style) and copies the raw last-sample
--   L/R (SynthFx.sfSampleRef, [-1,1]) into sampleL/sampleR.
stepRms :: World -> IO World
stepRms w0 = do
    mfx <- readIORef (synthFxRef w0)
    (rawL, rawR) <- case mfx of
        Just fx  -> readIORef (sfRmsRef fx)
        Nothing  -> return (0.0, 0.0)
    (sampL, sampR) <- case mfx of
        Just fx  -> readIORef (sfSampleRef fx)
        Nothing  -> return (0.0, 0.0)
    let kAtk = 0.2
        kRel = 0.03
        up v old = old + (if v > old then kAtk else kRel) * (v - old)
        kSm = 0.01
        smUp v old = old + kSm * (v - old)
    -- In-place ring-buffer write via unsafeThaw/unsafeFreeze (no 1440-element
    -- copy per tick).  Safe: the only reader is drawRelativeFrame on the SAME
    -- Gloss thread (Render/Example.hs reads sampleLRCache and materialises
    -- it with V.toList — the Picture never holds the Vector), and no other
    -- thread touches this field, so GHC sharing cannot alias a stale copy.
    -- Keep that invariant: never read sampleLRCache off the GUI thread.
    mvec <- V.unsafeThaw (sampleLRCache w0)
    VM.write mvec (sampleLRindex w0) (smUp sampL (sampleL w0))
    VM.write mvec (sampleLRindex w0 + 1) (smUp sampR (sampleR w0))
    samples' <- V.unsafeFreeze mvec
    return w0 { rmsSmL = up rawL (rmsSmL w0)
    , rmsSmR = up rawR (rmsSmR w0)
    , sampleL = smUp sampL (sampleL w0)
    , sampleR = smUp sampR (sampleR w0)
    , sampleLRindex = (sampleLRindex w0 + 2) `mod` (sampleLRsize w0)
    , sampleLRCache = samples' }

step :: TVar World -> TQueue MidiEvent -> Float -> World -> IO World
step w0TVar pollQue dT w0 = do
    stepTime dT w0
    >>= stepAngle
    >>= stepTuning
    >>= stepPreset
    >>= drainMidi pollQue
    >>= cleanBuffer 
    >>= stepChord
    >>= stepRms
    >>= writeWorld' w0TVar -- actually updates it, not overwrites it
-- This flow of world demonstrates "Functions are poor men's Object"
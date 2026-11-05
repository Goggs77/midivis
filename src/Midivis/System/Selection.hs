module Midivis.System.Selection
    ( startTuningSelection
    , stopTuningSelection
    , advanceTuningSelection
    , startPresetSelection
    , stopPresetSelection
    , advancePresetSelection
    ) where

import Data.IORef (readIORef, writeIORef)

import Midivis.Resources.Tuning.All (allTunings)
import Midivis.Synth.Preset (allPresets)
import Midivis.Synth.Render (SynthFx(..), pushVoiceToFx)
import Midivis.Tuning.Ascl (calibrateA4, hasReferencePitch)
import Midivis.World

-- | Delay before a held key starts repeating, then the repeat cadence.
--   Both use simulation time, so behavior is independent of frame
--   alignment and the GLUT backend's disabled OS key repeat.
initialRepeatDelay, repeatInterval :: Float
initialRepeatDelay = 0.32
repeatInterval = 0.075

startTuningSelection :: TuningScroll -> World -> IO World
startTuningSelection direction w0 =
    applyTuningStep direction (time w0 + initialRepeatDelay) w0

stopTuningSelection :: World -> IO World
stopTuningSelection w0 = pure w0 { tuningScroll = Stop }

advanceTuningSelection :: World -> IO World
advanceTuningSelection w0
    | tuningScroll w0 == Stop = pure w0
    | time w0 < tuningRepeatAt w0 = pure w0
    | otherwise =
        applyTuningStep (tuningScroll w0) (time w0 + repeatInterval) w0

applyTuningStep :: TuningScroll -> Float -> World -> IO World
applyTuningStep direction nextRepeat w0 = do
    let count = length allTunings
        delta = case direction of
            Next -> 1
            Prev -> -1
            Stop -> 0
        newIndex = (sclIndex w0 + delta + count) `mod` count
        tun = allTunings !! newIndex
        chosen =
            if hasReferencePitch tun
                then tun
                else calibrateA4 tun (freqA4 w0)
    pure w0
        { tuningScroll = direction
        , tuningRepeatAt = nextRepeat
        , sclIndex = newIndex
        , sclOfChoice = chosen
        }

startPresetSelection :: TuningScroll -> World -> IO World
startPresetSelection direction w0 =
    applyPresetStep direction (time w0 + initialRepeatDelay) w0

stopPresetSelection :: World -> IO World
stopPresetSelection w0 = pure w0 { presetScroll = Stop }

advancePresetSelection :: World -> IO World
advancePresetSelection w0
    | presetScroll w0 == Stop = pure w0
    | time w0 < presetRepeatAt w0 = pure w0
    | otherwise =
        applyPresetStep (presetScroll w0) (time w0 + repeatInterval) w0

applyPresetStep :: TuningScroll -> Float -> World -> IO World
applyPresetStep direction nextRepeat w0 = do
    let count = length allPresets
        delta = case direction of
            Next -> 1
            Prev -> -1
            Stop -> 0
        newIndex = (presetIndex w0 + delta + count) `mod` count
        newVoice = allPresets !! newIndex
    mfx <- readIORef (synthFxRef w0)
    case mfx of
        Just fx -> do
            writeIORef (sfVoice fx) newVoice
            pushVoiceToFx newVoice fx
        Nothing -> pure ()
    pure w0
        { presetScroll = direction
        , presetRepeatAt = nextRepeat
        , presetIndex = newIndex
        , activeVoice = newVoice
        }

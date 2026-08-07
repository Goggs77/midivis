module Midivis.System.ExampleRenderer where
import Midivis.Render.Example
import Midivis.EventHandler
import Midivis.System.MidiQuery
import Midivis.World 
import Midivis.Util.Math

import Graphics.Gloss.Relative
import Graphics.Gloss.Interface.Environment
import Sound.RtMidi (InputDevice)
import Control.Concurrent.STM
import qualified Data.StorableVector as SV
import Midivis.Tuning.Ascl (calibrateA4, hasReferencePitch)
import Midivis.Resources.Tuning.All




drawExampleRelative :: TVar World -> IO ()
drawExampleRelative w0TVar = do
    inputDevice <- initMidi
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
        1000 -- poll per 1 ms (1KHz)
        w0
        drawRelativeFrame
        handleEvent
        (step w0TVar inputDevice)

-- | Time management and modulation update
stepTime :: Float -> World -> IO World
stepTime dT w0 = 
    return w0{
        time = time w0 + dT, 
        totalFrames = totalFrames w0 + 1,
        fixedFrameCounter = if fixedFrameCounter w0 >= 1.0 / fromIntegral (fixedFrameRate w0) 
            then 0 
            else fixedFrameCounter w0 + dT
        }

stepAngle :: World -> IO World
stepAngle w0 = if isFixedFrame w0 then return w0{
    bufferVelocity = lerp (bufferVelocity w0) (clockVelocity w0 /(0.3+1.5*(fromIntegral (midiLength w0) - 0.1) )) 0.02,
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

step :: TVar World -> Maybe InputDevice -> Float -> World -> IO World
step w0TVar inputDevice dT w0 = do
    stepTime dT w0
    >>= stepAngle
    >>= stepTuning
    >>= bufferMidi inputDevice
    >>= cleanBuffer 
    >>= writeWorld' w0TVar -- actually updates it, not overwrites it
-- This flow of world demonstrates "Functions are poor men's Object"
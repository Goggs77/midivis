module Midivis.System.ExampleRenderer where
import Midivis.Render.Example
import Midivis.EventHandler
import Midivis.System.MidiQuery
import Midivis.World 
import Midivis.Util.Math

import Graphics.Gloss.Relative
import Sound.RtMidi (InputDevice)



drawExampleRelative :: World -> IO ()
drawExampleRelative w0 = do
    inputDevice <- initMidi
    putStrLn "[Debug] Midi Initialized"
    playRelativeIO
        (InWindow "Example Relative Window" (1151,809) (100,100))
        black
        1000 -- poll per 1 ms (1KHz)
        w0
        drawRelativeFrame
        handleEvent
        (step inputDevice)



stepTime :: Float -> World -> IO World
stepTime dT w0 = return w0{
    time = time w0 + dT, 
    fixedFrameCounter = if fixedFrameCounter w0 >= 1.0 / fromIntegral (fixedFrameRate w0) 
        then 0 
        else fixedFrameCounter w0 + dT
    }

stepAngle :: World -> IO World
stepAngle w0 = if isFixedFrame w0 then return w0{
    bufferVelocity = lerp (bufferVelocity w0) (clockVelocity w0 / fromIntegral (1+midiLength w0)) 0.02,
    baseAngle = baseAngle w0 + bufferVelocity w0
    } 
    else return w0

step :: InputDevice -> Float -> World -> IO World
step inputDevice dT w0 = do
    stepTime dT w0
    >>= stepAngle
    >>= bufferMidi inputDevice
    >>= cleanBuffer 
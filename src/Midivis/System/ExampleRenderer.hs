module Midivis.System.ExampleRenderer where
import Midivis.Render.Example
import Midivis.EventHandler
import Midivis.System.MidiQuery
import Midivis.World 

import Graphics.Gloss.Relative
import Sound.RtMidi (InputDevice)


drawExampleRelative :: World -> IO ()
drawExampleRelative w0 = do
    inputDevice <- initMidi
    putStrLn "[Debug] Midi Initialized"
    playRelativeIO
        (InWindow "Example Relative Window" (800,600) (100,100))
        black
        1000
        w0
        drawRelativeFrame
        handleEvent
        (step inputDevice)

step :: InputDevice -> Float -> World -> IO World
step inputDevice dT w0 = do
    bufferMidi inputDevice w0 >>= cleanBuffer 
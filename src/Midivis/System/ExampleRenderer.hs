module Midivis.System.ExampleRenderer where
import Midivis.Render.Example
import Midivis.EventHandler
import Midivis.System.MidiQuery
import Midivis.World 

import Graphics.Gloss.Relative
import Sound.RtMidi (InputDevice)


drawExampleRelative :: World -> IO ()
drawExampleRelative w = do
    inputDevice <- initMidi
    putStrLn "[Debug] Midi Initialized"
    playRelativeIO
        (InWindow "Example Relative Window" (800,600) (100,100))
        black
        1000
        w
        drawRelativeFrame
        handleEvent
        (step inputDevice)

step :: InputDevice -> Float -> World -> IO World
step inputDevice dT w0 = do
    {-putStrLn $ "[Debug] Stepping"-}
    bufferMidi inputDevice w0 -- add midi event to buffer
    {-putStrLn $ "[Debug] Clearing buffer"-}
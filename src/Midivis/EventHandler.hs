module Midivis.EventHandler where
import Graphics.Gloss.Relative
import Midivis.World
import Sound.RtMidi

handleEvent :: Event -> World -> IO World
handleEvent _ w = return w
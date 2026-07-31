module Midivis.EventHandler where
import Graphics.Gloss.Relative

import Midivis.World



handleEvent :: Event -> World -> IO World
handleEvent (EventKey (Char '=') Down _ _) w0 = return w0 {
    tuningScroll = Next
    }

handleEvent (EventKey (Char '-') Down _ _) w0 = return w0 {
    tuningScroll = Prev
    }

handleEvent (EventKey (Char '=') Up _ _) w0 = return w0 {
    tuningScroll = Stop
    }

handleEvent (EventKey (Char '-') Up _ _) w0 = return w0 {
    tuningScroll = Stop
    }

handleEvent _ w0 = return w0
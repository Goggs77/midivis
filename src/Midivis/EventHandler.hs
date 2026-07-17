module Midivis.EventHandler where
import Apecs
import Apecs.Gloss
import Midivis.World

handleEvent :: Event -> System' ()
handleEvent _ = return ()
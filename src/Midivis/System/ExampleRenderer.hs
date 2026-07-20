module Midivis.System.ExampleRenderer where
import Midivis.Render.Example
import Midivis.EventHandler
import Midivis.World
import Apecs.Gloss

draw = return drawExampleText

step :: Float -> System' ()
step _ = return ()
--play (InWindow "Title" (Dimension) (Position)) BGColor fps renderer eventHandler updateTick
drawExampleWindow = play (InWindow "Example Window" (800, 600) (100,100)) black 60 draw handleEvent step
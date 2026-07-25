module Main where

import Midivis.World
import Midivis.System.ExampleRenderer 

main :: IO ()
main = do
    let w0 = initWorld
    drawExampleRelative w0



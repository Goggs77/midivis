module Main where

import Midivis.World
import Midivis.System.ExampleRenderer 

main :: IO ()
main = do
    w <- initWorld
    chan <- newChan
    drawExampleRelative w chan



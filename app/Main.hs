module Main where

import Midivis.World
import Midivis.System.ExampleRenderer 
import Control.Concurrent.STM

main :: IO ()
main = do
    -- now supports multi threads with World access, need to add audio thread
    w0TVar <- newTVarIO initWorld
    drawExampleRelative w0TVar



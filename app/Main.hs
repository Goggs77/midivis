module Main where

import Midivis.World
import Midivis.System.ExampleRenderer (drawExampleWindow)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  initWorld >>= runSystem drawExampleWindow
    


module Midivis.Render.Example where

import Graphics.Gloss.Relative

import Midivis.World

rgba :: Float -> Float -> Float -> Float -> Color
rgba = makeColor
rgbaI :: Int -> Int -> Int -> Int -> Color
rgbaI = makeColorI

drawExampleText :: Picture
drawExampleText =  (scale 0.1 0.1 . color white) (Text "This is an example window")

drawRelativeFrame :: World -> IO Frame
drawRelativeFrame w = return $ 
    
    zoom (Relative 0.5) (Relative 0.5) alignCenter $ 
        thickEllipse (rgba 1 1 1 1) (Absolute 1.0)
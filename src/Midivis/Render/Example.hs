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
drawRelativeFrame w0 = do 
    let centerPoint = Aspect (1,1) alignCenter $
            zoom (Absolute 2) (Absolute 2) alignCenter $
            solidEllipse (rgba 1 1 1 1)
        middleCircle = Aspect (1,1) alignCenter $ 
            zoom (Relative 0.73) (Relative 0.73) alignCenter $ 
            thickEllipse (rgba 1 1 1 0.8) (Absolute 2.0)
    return $ Overlay [centerPoint, middleCircle]


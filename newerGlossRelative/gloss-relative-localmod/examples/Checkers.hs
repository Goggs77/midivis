-- | Checkers board — a resolution-independent board laid out relative to the screen.
--
-- __Exercises:__ 'aspect', 'overlay', 'zoom', 'grid', 'solid', 'makeColorI', 'displayRelative'.
module Main where

import Graphics.Gloss.Relative

lightWood :: Color
lightWood = makeColorI 222 184 135 255

darkWood :: Color
darkWood = makeColorI 62 49 49 255

draw :: Frame
draw = aspect (11,11) alignCenter $ overlay [border,slots]
    where
    border = solid darkWood
    slots  = zoom (Relative (10/11)) (Relative (10/11)) alignCenter $ grid 10 10 drawSlot

drawSlot :: Int -> Int -> Frame
drawSlot i j = if even i == even j
    then solid white
    else solid black

main :: IO ()
main = displayRelative FullScreen lightWood draw

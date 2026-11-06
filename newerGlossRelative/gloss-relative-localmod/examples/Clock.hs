-- | Clock — an animated analog clock built entirely from frame primitives (no raw 'Picture').
--
-- The hands are just 'stroke's whose endpoints are recomputed from the elapsed time, so no picture-rotation is needed.
--
-- __Exercises:__ 'animateRelative', 'aspect', 'overlay', 'solidEllipse', 'thickEllipse', 'stroke', 'UnitPoint'.
module Main where

import Graphics.Gloss.Relative

window :: Display
window = InWindow "gloss-relative: Clock" (500, 500) (10, 10)

-- | A point at distance @r@ (fraction of the frame) and angle @a@ (radians, 0 = 12 o'clock).
radial :: Float -> Float -> UnitPoint
radial r a = (Relative (r * sin a), Relative (r * cos a))

hand :: Float -> Color -> Float -> Frame
hand len c turns = stroke [(Relative 0, Relative 0), radial len (2 * pi * turns)] c

draw :: Float -> Frame
draw t = aspect (1, 1) alignCenter $ overlay $
    [ solidEllipse (greyN 0.97)
    , thickEllipse black (Relative 0.02)
    ] ++
    [ stroke [radial 0.42 a, radial 0.48 a] (greyN 0.4) | k <- [0 .. 11 :: Int], let a = 2 * pi * fromIntegral k / 12 ] ++
    [ hand 0.25 black (t / 43200)  -- hours   (1 turn / 12h)
    , hand 0.38 blue  (t / 3600)   -- minutes (1 turn / hour)
    , hand 0.44 red   (t / 60)     -- seconds (1 turn / minute)
    ]

main :: IO ()
main = animateRelative window white draw

-- | Units — Relative vs Absolute sizing under window resize (resize the window to compare).
--
-- The left box keeps a fixed /fraction/ of its pane; the right box keeps a fixed /pixel/ size.
--
-- __Exercises:__ 'Relative' vs 'Absolute' 'Unit's, 'splitLeft', 'splitBottom', 'background', 'displayRelative'.
module Main where

import Graphics.Gloss.Relative

window :: Display
window = InWindow "gloss-relative: Units (resize me!)" (720, 480) (10, 10)

draw :: Frame
draw = splitLeft (Relative 0.5)
    (pane "Relative 50%" (zoom (Relative 0.5) (Relative 0.5) alignCenter (solid azure)))
    (pane "Absolute 150px" (zoom (Absolute 150) (Absolute 150) alignCenter (solid orange)))
  where
    pane title f = background (greyN 0.9)
        $ splitBottom (Absolute 44) (inset (Relative 0.06) f) (banner title black)

main :: IO ()
main = displayRelative window white draw

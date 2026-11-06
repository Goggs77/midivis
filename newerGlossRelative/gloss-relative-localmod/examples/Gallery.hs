-- | Gallery — a static showcase of every native leaf primitive plus a few decorators, using no raw 'Picture'.
--
-- __Exercises:__ 'solid', 'outline', 'ellipse', 'solidEllipse', 'thickEllipse', 'shape', 'stroke', 'bordered', 'grid', 'splitBottom', 'banner', 'displayRelative'.
module Main where

import Graphics.Gloss.Relative

window :: Display
window = InWindow "gloss-relative: Gallery" (900, 620) (10, 10)

-- | A demo frame captioned with its name below it.
cell :: String -> Frame -> Frame
cell name f = inset (Relative 0.05)
    $ splitBottom (Relative 0.2) (inset (Relative 0.06) f) (banner name (greyN 0.3))

demos :: [Frame]
demos =
    [ cell "solid"        (solid azure)
    , cell "outline"      (outline red)
    , cell "ellipse"      (ellipse violet)
    , cell "solidEllipse" (solidEllipse orange)
    , cell "thickEllipse" (thickEllipse chartreuse (Relative 0.15))
    , cell "shape"        (shape triangle rose)
    , cell "stroke"       (stroke cross blue)
    , cell "bordered"     (bordered (Absolute 8) black (solid aquamarine))
    ]
  where
    triangle = [(Relative 0, Relative 0.5), (Relative 0.5, Relative (-0.5)), (Relative (-0.5), Relative (-0.5))]
    cross    = [(Relative (-0.5), Relative (-0.5)), (Relative 0.5, Relative 0.5), (Relative 0, Relative 0), (Relative (-0.5), Relative 0.5), (Relative 0.5, Relative (-0.5))]

draw :: Frame
draw = background (greyN 0.92) $ grid 4 2 pick
  where
    pick r c = let i = r * 4 + c
               in if i < length demos then demos !! i else empty

main :: IO ()
main = displayRelative window white draw

module Midivis.Util.Math where
import Graphics.Gloss.Relative

scaleNum :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNum inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(x - inf1)/(sup1 - inf1) + inf2) 

scaleNumLog :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNumLog inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(log x - log inf1)/(log sup1 - log inf1) + inf2) 

lerp :: Double -> Double -> Double -> Double
lerp from to factor = from + (to-from)*factor

(.*) :: UnitPoint -> Float -> UnitPoint
(.*) (Relative x, Relative y) f = (Relative (x*f), Relative (y*f))
(.*) (Absolute x, Absolute y) f = (Absolute (x*f), Absolute (y*f))
(.*) (Absolute x, Relative y) f = (Absolute (x*f), Relative (y*f))
(.*) (Relative x, Absolute y) f = (Relative (x*f), Absolute (y*f))
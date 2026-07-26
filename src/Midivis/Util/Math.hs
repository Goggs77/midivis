module Midivis.Util.Math where

scaleNum :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNum inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(x - inf1)/(sup1 - inf1) + inf2) 

scaleNumLog :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNumLog inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(log x - log inf1)/(log sup1 - log inf1) + inf2) 

lerp :: Double -> Double -> Double -> Double
lerp from to factor = from + (to-from)*factor
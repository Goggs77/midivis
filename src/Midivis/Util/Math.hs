module Midivis.Util.Math where
import Graphics.Gloss.Relative

ln2 :: Double
ln2 = 0.6931471805599453

ln10 :: Double
ln10 = 2.302585092994046

log2 :: Double -> Double
log2 x = log x / ln2

log10 :: Double -> Double
log10 x = log x / ln10

rad2deg :: Double -> Double
rad2deg = (57.29577951308232*)

deg2rad :: Double -> Double
deg2rad = (1.7453292519943295e-2*)

scaleNum :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNum inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(x - inf1)/(sup1 - inf1) + inf2) 

scaleNumLog :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNumLog inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(log x - log inf1)/(log sup1 - log inf1) + inf2) 

scaleNumLog2 :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNumLog2 inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(log2 x - log2 inf1)/(log2 sup1 - log2 inf1) + inf2)

scaleLog2 :: Double -> Double -> Double -> Double -> Double -> Double
scaleLog2 inf1 sup1 inf2 sup2 = (\x -> (sup2 - inf2)*(log2 x - log2 inf1)/(log2 sup1 - log2 inf1) + inf2)

scaleNumLog10 :: Double -> Double -> Double -> Double -> [Double] -> [Double]
scaleNumLog10 inf1 sup1 inf2 sup2 = map (\x -> (sup2 - inf2)*(logBase 10 x - logBase 10 inf1)/(logBase 10 sup1 - logBase 10 inf1) + inf2) 

lerp :: Double -> Double -> Double -> Double
lerp from to factor = from + (to-from)*factor

-- | Get delta in a power function in [0, 1] with scaling [from, to]
deltaPower :: Double -> Double ->  Float -> Float -> Double -> Double
deltaPower from to curve deltaRatio offsetY = let
    power = 10.0 ** (2.0 * realToFrac curve - 1.0) -- center 0.5 expmap
    range = to - from
    offsetRatio = offsetY ** (1 / power) -- offsetY \in [0, 1], not a bug yet
    in range * ((realToFrac deltaRatio + offsetRatio) ** power - offsetRatio ** power)

fromDBFS :: Double -> Double
fromDBFS dBFS = 10.0 ** (dBFS / 20.0) -- fixed sign issues

toDBFS :: Double -> Double
toDBFS linear = log10 linear * (-20.0)

zeroRelative :: UnitPoint
zeroRelative = (Relative 0, Relative 0)

(.*) :: UnitPoint -> Float -> UnitPoint
(.*) (Relative x, Relative y) f = (Relative (x*f), Relative (y*f))
(.*) (Absolute x, Absolute y) f = (Absolute (x*f), Absolute (y*f))
(.*) (Absolute x, Relative y) f = (Absolute (x*f), Relative (y*f))
(.*) (Relative x, Absolute y) f = (Relative (x*f), Absolute (y*f))

tf21 :: (a -> b -> c -> d) -> (b -> a -> c -> d)
tf21 f b a = f a b

module Midivis.Util.Math where
import Graphics.Gloss.Relative
import Data.Fixed (mod')
import Data.Bits
import Data.Word
import GHC.Float (castDoubleToWord64)

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

fromIntegralPoint :: (Integral a, Num b) => (a, a) -> (b, b)
fromIntegralPoint (x, y) = (fromIntegral x, fromIntegral y)

-- | Converts an interval to frequency ratio, using 12TET. It's inverse is @to12TET@
from12TET :: Double -> Double
from12TET interval = 2.0 ** (interval / 12.0)

-- | The inverse function of @from12TET@
to12TET :: Double -> Double
to12TET ratio = 12.0 * log2 ratio

infix 4 <->
(<->) :: [a -> b] -> [a] -> [b]
_ <-> [] = []
[] <-> _ = []
f:fs <-> x:xs = f x : (fs <-> xs)

-- | Use radian for h
hsv2rgb :: Double -> Double -> Double -> Color
hsv2rgb h s v = 
    let
        c = s*v
        h' = h*3.0/pi
        x = c*(1 - abs ((h' `mod'` 2) - 1))
        m = v - c
        (r, g, b) = case h' of
            n | 0 <= n && n < 1 -> forT3 (c, x, 0) (+m)
            n | 1 <= n && n < 2 -> forT3 (x, c, 0) (+m)
            n | 2 <= n && n < 3 -> forT3 (0, c, x) (+m)
            n | 3 <= n && n < 4 -> forT3 (0, x, c) (+m)
            n | 4 <= n && n < 5 -> forT3 (x, 0, c) (+m)
            n | 5 <= n && n < 6 -> forT3 (c, 0, x) (+m)
            _ -> forT3 (c, x, 0) (+m)
    in makeColor (realToFrac r) (realToFrac g) (realToFrac b) 0.95


forT3 :: (Double, Double, Double) -> (Double -> Double) -> (Double, Double, Double)
forT3 (a, b, c) f = (f a, f b, f c)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- | LCG normalized to (-0.01, +0.01)
detNoise :: Int -> Int -> Double
detNoise s slot =
  let h = (fromIntegral s * 2654435761 + fromIntegral (slot * 7919 + 17) * 40503) * 1103515245 :: Int
      u = fromIntegral ((h `shiftR` 16) .&. 0xFFFFFF) :: Double
   in (u / 8388607.5 - 1.0) * 0.01

-- | bitwise cast Double to Int
d2i :: Double -> Int
d2i d = 
    let b = fromIntegral (castDoubleToWord64 d) :: Word64
    in fromIntegral (b .|. 0xFFFFFFFF)
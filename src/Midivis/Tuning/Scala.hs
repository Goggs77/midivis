{-# OPTIONS_GHC -Wno-type-defaults #-}
module Midivis.Tuning.Scala where
    
data Scala = Scala
    {
        name :: String, -- find Name Literals at Resources.Tuning, where they are set during TH
        lowestFreq :: Double,
        synopsis :: String,
        count :: Integer,
        pitches :: [Double]
    }

c0 :: Double
c0 = 16.351597831287414

calibrate :: Integral a => Scala -> a -> Double -> Scala
calibrate scl noteID newFreq =
    scl {lowestFreq = newLow} where
        (q, r) = fromIntegral noteID `quotRem` count scl
        prod = if r /= 0 then pitches scl !! fromIntegral (r - 1) else 1
        newLow = newFreq / (prod * 2.0 ^^ q)

calibrateA4 :: Scala -> Double -> Scala
calibrateA4 scl newFreq = calibrate scl 57 newFreq

calibrateC4 :: Scala -> Double -> Scala
calibrateC4 scl newFreq = calibrate scl 48 newFreq

getFreq :: Integral a => Scala -> a -> Double
getFreq scl noteID = freq where
    (q, r) = fromIntegral noteID `quotRem` count scl
    prod = if r /= 0 then pitches scl !! fromIntegral (r - 1) else 1
    freq = lowestFreq scl * (prod * 2.0 ^^ q)

getGeneralName :: Integral a => Scala -> a -> String
getGeneralName scl noteID =
    let (q, r) = fromIntegral noteID `quotRem` count scl
        noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
    in if name scl == "EDO12"
       then (noteNames !! fromIntegral r) ++ show q
       else show r ++ "/" ++ show q

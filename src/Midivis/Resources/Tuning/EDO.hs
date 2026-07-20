module Midivis.Resources.Tuning.EDO where
import Midivis.Tuning.TuningParser (Tuning)

data EDO = EDO
    {
        denominator :: Integer,-- why Integer? well i just want 9quntillion EDO
        lowestFreq :: Double --frequency for C0 (the note 0); negative frequency is phase negation
    } 
instance Tuning EDO where
-- given a EDO, define a frequency for a specific note(Int ID, e.g. C4 == 60), and returun the callibrated EDO
calibreate ::Integral a => EDO -> a -> Double -> EDO
calibreate (EDO den _) note freq = --this equation: newlow*2**(note/den) == freq
    let
        newlow = freq/(2.0**(fromIntegral note/fromIntegral den))
    in EDO den newlow

calibreateA4 :: EDO -> Double -> EDO
calibreateA4 e = calibreate e 69

calibreateC4 :: EDO -> Double -> EDO
calibreateC4 e = calibreate e 60

getFreq :: Integral a => EDO -> a -> Double
getFreq (EDO den l) note = l*(2.0**(fromIntegral note/fromIntegral den))

--get the higher tone over the lower tone, over a range of some semitones
getRatio :: Integral a => EDO -> a -> Double 
getRatio (EDO den _) diff = 2.0**(fromIntegral diff/ fromIntegral den)

getGeneralName :: Integral a => EDO -> a -> String
getGeneralName (EDO den _) note =
      let (q, r) = fromIntegral note `quotRem` fromIntegral den
      in show r ++ "/" ++ show q --allow q=0
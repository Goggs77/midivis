{-# LANGUAGE InstanceSigs #-}
module Midivis.Resources.Tuning.Default where
import Midivis.Tuning.TuningParser ( Tuning(..) )
-- 12 EDO, or 12 TET
newtype EDO12 = EDO12 {lowestFreq :: Double}
instance Tuning EDO12 where

    period :: EDO12 -> Integer
    period _ = 12

    lowestFreq :: EDO12 -> Double
    lowestFreq (EDO12 l) = l

    calibrate :: Integral b => EDO12 -> b -> Double -> EDO12
    calibrate (EDO12 _) note freq = --this equation: newlow*2**(note/12) == freq
        let
            newlow = freq/(2.0**(fromIntegral note/12.0))
        in EDO12 newlow

    calibrateA4 :: EDO12 -> Double -> EDO12
    calibrateA4 (EDO12 l) = calibrate (EDO12 l) 69

    calibrateC4 :: EDO12 -> Double -> EDO12
    calibrateC4 (EDO12 l) = calibrate (EDO12 l) 60

    getFreq :: Integral a => EDO12 -> a -> Double
    getFreq (EDO12 l) note = l*(2.0**(fromIntegral note/12.0))

    
    getGeneralName :: Integral a => EDO12 -> a -> String
    getGeneralName _ note =
        let
            (q, r) = fromIntegral note `quotRem` 12
            name = case r of
                0 -> "C"
                1 -> "C#"
                2 -> "D"
                3 -> "D#"
                4 -> "E"
                5 -> "F"
                6 -> "F#"
                7 -> "G"
                8 -> "G#"
                9 -> "A"
                10 -> "A#"
                11 -> "B"
                _ -> error "There must have been a program messing with memory, otherwise a cosmic ray hit your computer!"
        in
            name ++ show q

--get the higher tone over the lower tone, over a range of some semitones
getRatio :: Integral a => EDO12 -> a -> Double 
getRatio (EDO12 _) diff = 2.0**(fromIntegral diff/12.0)

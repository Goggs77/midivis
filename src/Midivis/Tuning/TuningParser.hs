module Midivis.Tuning.TuningParser where
--MIDI supports [0 127] note range. we fit each note periodically
class Tuning a where
    calibreate :: Integral b => a -> b -> Double -> a
    calibreateA4 :: a -> Double -> a
    calibreateC4 :: a -> Double -> a
    getFreq :: Integral b => a -> b -> Double
    getGeneralName :: Integral b => a -> b -> String
module Main (main) where

import Midivis.Tuning.Scala

main :: IO ()
main = do
    let tuning = Scala "test" 1 "test tuning" 2 [2]
        calibrated = calibrate tuning 1 440
    if abs (getFreq calibrated 1 - 440) < 1e-9
        then pure ()
        else error "calibrate/getFreq should preserve the requested frequency"

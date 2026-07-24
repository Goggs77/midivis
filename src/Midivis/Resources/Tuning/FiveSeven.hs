{-# LANGUAGE TemplateHaskell #-}
module Midivis.Resources.Tuning.FiveSeven where
import Data.FileEmbed
import Midivis.Tuning.TuningParser 

$(makeTuning "FiveSeven" (parseScala $(embedFileRelative "assets/tuning/7-EDO + 5-EDO.scl")))
-- Actually, there's no type mismatch afterall, reboot HLS solved it
{-# LANGUAGE TemplateHaskell #-}
module Midivis.Resources.Tuning.FiveSeven where
import Data.FileEmbed
import Midivis.Tuning.TuningParser 

$(makeScala "FiveSeven" (parseScala $(embedFileRelative "assets/tuning/debug.scl")))
-- Actually, there's no type mismatch afterall, reboot HLS solved it
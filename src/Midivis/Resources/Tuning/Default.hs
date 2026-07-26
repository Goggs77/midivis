{-# LANGUAGE TemplateHaskell #-}
module Midivis.Resources.Tuning.Default where
import Data.FileEmbed
import Midivis.Tuning.TuningParser
-- 12 EDO, or 12 TET

$(makeScala "DefaultTuning" (parseScala $(embedFileRelative "assets/tuning/12-TET(EDO).scl")))

{-# LANGUAGE TemplateHaskell #-}
module Midivis.Resources.Tuning.JustIntonation where
import Data.FileEmbed
import Midivis.Tuning.TuningParser

$(makeScala "Quintal12" (parseScala $(embedFileRelative "assets/tuning/12 Quintal (center C).scl")))
$(makeScala "Tertial_Quintal_Sabat_Euler_Lattice_55" (parseScala $(embedFileRelative "assets/tuning/55 Tertial-Quintal Sabat Euler Lattice.scl")))
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Midivis.Resources.Tuning.JustIntonation where
import Data.FileEmbed
import Midivis.Tuning.TuningParser

$(makeAscl "Quintal12" (parseAscl $(embedFileRelative "assets/tuning/12 Quintal (center C).scl")))
$(makeAscl "Tertial_Quintal_Sabat_Euler_Lattice_55" (parseAscl $(embedFileRelative "assets/tuning/55 Tertial-Quintal Sabat Euler Lattice.scl")))
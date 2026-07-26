{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Midivis.Resources.Tuning.Dastgah where
import Data.FileEmbed
import Midivis.Tuning.TuningParser
$(makeScala "Dastgāh_e_Abuatā" (parseScala $(embedFileRelative "assets/tuning/Dastgah-e Abuata.scl")))
{-
keep file name ASCII-compliant to sensure build success
-}
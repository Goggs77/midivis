{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Midivis.Resources.Tuning.EDO where
import Data.FileEmbed
import Midivis.Tuning.TuningParser

$(makeScala "EDO5" (parseScala $(embedFileRelative "assets/tuning/5-EDO.scl")))
$(makeScala "EDO7" (parseScala $(embedFileRelative "assets/tuning/7-EDO.scl")))
$(makeScala "EDO31" (parseScala $(embedFileRelative "assets/tuning/31-EDO.scl")))
$(makeScala "EDO41" (parseScala $(embedFileRelative "assets/tuning/41-EDO.scl")))
$(makeScala "Bayati_Saba_Hijaz_Kurd_24EDO" (parseScala $(embedFileRelative "assets/tuning/Bayati-Saba-Hijaz-Kurd 24EDO.scl")))
$(makeScala "FiveSeven" (parseScala $(embedFileRelative "assets/tuning/7-EDO + 5-EDO.scl")))
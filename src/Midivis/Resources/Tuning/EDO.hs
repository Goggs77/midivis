{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Midivis.Resources.Tuning.EDO where
import Data.FileEmbed
import Midivis.Tuning.TuningParser

$(makeAscl "EDO5" (parseAscl $(embedFileRelative "assets/tuning/5-EDO.scl")))
$(makeAscl "EDO7" (parseAscl $(embedFileRelative "assets/tuning/7-EDO.scl")))
$(makeAscl "EDO31" (parseAscl $(embedFileRelative "assets/tuning/31-EDO.scl")))
$(makeAscl "EDO41" (parseAscl $(embedFileRelative "assets/tuning/41-EDO.scl")))
$(makeAscl "Bayati_Saba_Hijaz_Kurd_24EDO" (parseAscl $(embedFileRelative "assets/tuning/Bayati-Saba-Hijaz-Kurd 24EDO.scl")))
$(makeAscl "FiveSeven" (parseAscl $(embedFileRelative "assets/tuning/7-EDO + 5-EDO.scl")))
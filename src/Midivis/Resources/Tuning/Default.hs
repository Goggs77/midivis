{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Midivis.Resources.Tuning.Default where
import Data.FileEmbed
import Midivis.Tuning.TuningParser
-- 12 EDO, or 12 TET

$(makeAscl "EDO12" (parseAscl $(embedFileRelative "assets/tuning/12-TET(EDO).scl")))

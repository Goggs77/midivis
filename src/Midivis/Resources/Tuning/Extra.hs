{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | Recently added tunings (2026-08-07).  All files carry @ABL directives;
--   grouped by temperament family.  @count@ = notes per octave (音程数).
module Midivis.Resources.Tuning.Extra where
import Data.FileEmbed
import Midivis.Tuning.TuningParser

--------------------------------------------------------------------------------
-- EDO (equal divisions of the octave)
--------------------------------------------------------------------------------
$(makeAscl "EDO11" (parseAscl $(embedFileRelative "assets/tuning/11-EDO.ascl")))
$(makeAscl "EDO13" (parseAscl $(embedFileRelative "assets/tuning/13-EDO.ascl")))
$(makeAscl "EDO19" (parseAscl $(embedFileRelative "assets/tuning/19-EDO.ascl")))
$(makeAscl "EDO22" (parseAscl $(embedFileRelative "assets/tuning/22-EDO.ascl")))
$(makeAscl "EDO24" (parseAscl $(embedFileRelative "assets/tuning/24-EDO.ascl")))
$(makeAscl "EDO35" (parseAscl $(embedFileRelative "assets/tuning/35-EDO.ascl")))
$(makeAscl "EDO36" (parseAscl $(embedFileRelative "assets/tuning/36-EDO.ascl")))
$(makeAscl "EDO43" (parseAscl $(embedFileRelative "assets/tuning/43-EDO.ascl")))
$(makeAscl "EDO53" (parseAscl $(embedFileRelative "assets/tuning/53-EDO.ascl")))
$(makeAscl "EDO55" (parseAscl $(embedFileRelative "assets/tuning/55-EDO.ascl")))
$(makeAscl "EDO72" (parseAscl $(embedFileRelative "assets/tuning/72-EDO.ascl")))

--------------------------------------------------------------------------------
-- Non-octave equal divisions (Wendy Carlos alpha/beta/gamma, Bohlen-Pierce)
--------------------------------------------------------------------------------
$(makeAscl "WendyCarlosAlpha9" (parseAscl $(embedFileRelative "assets/tuning/9-ED3_2 Wendy Carlos alpha.ascl")))
$(makeAscl "WendyCarlosBeta11" (parseAscl $(embedFileRelative "assets/tuning/11-ED3_2 Wendy Carlos beta.ascl")))
$(makeAscl "WendyCarlosGamma20" (parseAscl $(embedFileRelative "assets/tuning/20-ED3_2 Wendy Carlos gamma.ascl")))
$(makeAscl "BohlenPierce13" (parseAscl $(embedFileRelative "assets/tuning/13-ED3 Bohlen Pierce.ascl")))

--------------------------------------------------------------------------------
-- Just intonation / historical temperaments
--------------------------------------------------------------------------------
$(makeAscl "Johnston12" (parseAscl $(embedFileRelative "assets/tuning/12 HS 16-32 (Johnston).ascl")))
$(makeAscl "Helmholtz24" (parseAscl $(embedFileRelative "assets/tuning/24 Helmholtz temperament.ascl")))
$(makeAscl "Partch43" (parseAscl $(embedFileRelative "assets/tuning/43 Undecimal Partch G-392Hz.ascl")))

--------------------------------------------------------------------------------
-- Maqam / Dastgah
--------------------------------------------------------------------------------
$(makeAscl "Bayati1" (parseAscl $(embedFileRelative "assets/tuning/Bayati 1.ascl")))
$(makeAscl "Rast1" (parseAscl $(embedFileRelative "assets/tuning/Rast 1 - Egypt mid 20th.ascl")))
$(makeAscl "DastgahNava" (parseAscl $(embedFileRelative "assets/tuning/Dastgah-e Nava.ascl")))
$(makeAscl "DastgahShur" (parseAscl $(embedFileRelative "assets/tuning/Dastgah-e Shur.ascl")))

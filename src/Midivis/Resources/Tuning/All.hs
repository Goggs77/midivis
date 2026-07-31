module Midivis.Resources.Tuning.All
    (
        module Midivis.Resources.Tuning.EDO
    ,   module Midivis.Resources.Tuning.Default
    ,   module Midivis.Resources.Tuning.JustIntonation
    ,   module Midivis.Resources.Tuning.Dastgah
    ,   allTunings
    ) where
import Midivis.Resources.Tuning.EDO
import Midivis.Resources.Tuning.Default
import Midivis.Resources.Tuning.JustIntonation
import Midivis.Resources.Tuning.Dastgah
import Midivis.Tuning.Scala ( Scala )

allTunings :: [Scala]
allTunings =
    [ edo5
    , edo7
    , edo31
    , edo41
    , bayati_Saba_Hijaz_Kurd_24EDO
    , fiveSeven
    , edo12
    , quintal12
    , tertial_Quintal_Sabat_Euler_Lattice_55
    , dastgāh_e_Abuatā
    ]

module Midivis.Resources.Tuning.All
    (
        module Midivis.Resources.Tuning.EDO
    ,   module Midivis.Resources.Tuning.Default
    ,   module Midivis.Resources.Tuning.JustIntonation
    ,   module Midivis.Resources.Tuning.Dastgah
    ,   module Midivis.Resources.Tuning.Extra
    ,   allTunings
    ) where
import Midivis.Resources.Tuning.EDO
import Midivis.Resources.Tuning.Default
import Midivis.Resources.Tuning.JustIntonation
import Midivis.Resources.Tuning.Dastgah
import Midivis.Resources.Tuning.Extra
import Midivis.Tuning.Ascl ( Ascl )

-- | All tunings, ascending by notes-per-octave (音程数).
allTunings :: [Ascl]
allTunings =
    [ edo5                                   -- 5
    , dastgāh_e_Abuatā                       -- 6
    , edo7                                   -- 7
    , dastgahNava                            -- 7
    , dastgahShur                            -- 7
    , wendyCarlosAlpha9                      -- 9
    , edo11                                  -- 11
    , wendyCarlosBeta11                      -- 11
    , edo12                                  -- 12
    , fiveSeven                              -- 12
    , quintal12                              -- 12
    , johnston12                             -- 12
    , bayati1                                -- 12
    , rast1                                  -- 12
    , edo13                                  -- 13
    , bohlenPierce13                         -- 13
    , bayati_Saba_Hijaz_Kurd_24EDO           -- 15
    , edo19                                  -- 19
    , wendyCarlosGamma20                     -- 20
    , edo22                                  -- 22
    , edo24                                  -- 24
    , helmholtz24                            -- 24
    , edo31                                  -- 31
    , edo35                                  -- 35
    , edo36                                  -- 36
    , edo41                                  -- 41
    , edo43                                  -- 43
    , partch43                               -- 43
    , edo53                                  -- 53
    , edo55                                  -- 55
    , tertial_Quintal_Sabat_Euler_Lattice_55 -- 55
    , edo72                                  -- 72
    ]

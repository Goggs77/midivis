{-# LANGUAGE OverloadedStrings #-}
module Midivis.Tuning.Ascl where
import qualified Data.Text as T
import Midivis.Util.Text
data Ascl = Ascl
    {
        tunName :: T.Text, -- find Name Literals at Resources.Tuning, where they are set during TH
        lowestFreq :: Double,
        synopsis :: T.Text,
        count :: Integer,
        pitches :: [Double],
        directives :: AsclDirectiveSet,
        hasReferencePitch :: Bool -- True when the file declares @ABL REFERENCE_PITCH explicitly
    }

data AsclDirectiveSet = DirectiveSet
    {
    referencePitch :: AsclDirective
    , noteNames :: AsclDirective
    , noteRange :: AsclDirective
    , tuningSource :: AsclDirective
    , sourceLink :: AsclDirective 
    }
--refPitch names range source link
unsafeGetNames :: Ascl -> [T.Text]
unsafeGetNames ascl = let d = noteNames $ directives ascl 
    in case d of 
        NOTE_NAMES t -> t
        _ -> error "Incorrect name position"

unsafeGetReferencePitch :: Ascl -> (Integer, Integer, Double)
unsafeGetReferencePitch ascl = let d = referencePitch $ directives ascl
    in case d of
        REFERENCE_PITCH o i f -> (o, i, f)
        _ -> error "Incorrect reference pitch position"

unsafeGetNoteRange :: Ascl -> Either (Double, Double) (Integer, Integer, Integer, Integer)
unsafeGetNoteRange ascl = let d = noteRange $ directives ascl
    in case d of
        NOTE_RANGE_BY_FREQUENCY minF maxF -> Left (minF, maxF)
        NOTE_RANGE_BY_INDEX minO minI maxO maxI -> Right (minO, minI, maxO, maxI)
        _ -> error "Incorrect note range position"

unsafeGetSource :: Ascl -> T.Text
unsafeGetSource ascl = let d = tuningSource $ directives ascl
    in case d of
        SOURCE s -> s
        _ -> error "Incorrect source position"

unsafeGetLink :: Ascl -> T.Text
unsafeGetLink ascl = let d = sourceLink $ directives ascl
    in case d of
        LINK l -> l
        _ -> error "Incorrect link position"

defDirectiveSet :: AsclDirectiveSet
defDirectiveSet = DirectiveSet defReferencePitch defNoteNames defNoteRange defSource defLink

defReferencePitch :: AsclDirective
defReferencePitch = REFERENCE_PITCH 3 9 440.0

defNoteNames :: AsclDirective
defNoteNames = NOTE_NAMES $ repeat "Unnamed Note"

defNoteRange :: AsclDirective
defNoteRange = NOTE_RANGE_BY_FREQUENCY 20.0 18000.0

defSource :: AsclDirective
defSource = SOURCE $ "Unknown Source"

defLink :: AsclDirective
defLink = LINK $ "This .ascl is shipped without source link."

data AsclDirective = 
    REFERENCE_PITCH {octave :: Integer, index :: Integer, freq :: Double}
    | NOTE_NAMES {names :: [T.Text]}
    | NOTE_RANGE_BY_FREQUENCY {min :: Double, max :: Double} -- max is optional, both \in [4.0, 21000.0]
    | NOTE_RANGE_BY_INDEX {minOctave :: Integer, minIndex :: Integer
                        , maxOctave :: Integer, maxIndex :: Integer} -- max is optional, octaves \in [-99..99]
    | SOURCE {source :: T.Text}
    | LINK {link :: T.Text}

c0 :: Double
c0 = 16.351597831287414

calibrate :: Integral a => Ascl -> a -> Double -> Ascl
calibrate ascl noteID newFreq =
    ascl {lowestFreq = newLow} where
        (q, r) = fromIntegral noteID `quotRem` count ascl
        prod = if r /= 0 then pitches ascl !! fromIntegral (r - 1) else 1
        newLow = newFreq / (prod * 2.0 ^^ q)

-- | Calibrate so standard MIDI note 69 (A4 = 440 Hz) plays @newFreq@.
calibrateA4 :: Ascl -> Double -> Ascl
calibrateA4 ascl = calibrate ascl 69

-- | Calibrate so standard MIDI note 60 (C4) plays @newFreq@.
calibrateC4 :: Ascl -> Double -> Ascl
calibrateC4 ascl = calibrate ascl 60

getFreq :: Integral a => Ascl -> a -> Double
getFreq ascl noteID = freq where
    (q, r) = fromIntegral noteID `quotRem` count ascl
    prod = if r /= 0 then pitches ascl !! fromIntegral (r - 1) else 1
    freq = lowestFreq ascl * (prod * 2.0 ^^ q)

getGeneralName :: Integral a => Ascl -> a -> T.Text
getGeneralName ascl noteID =
    let (q, r) = fromIntegral noteID `quotRem` count ascl
    in T.intercalate " " [(unsafeGetNames ascl !! fromIntegral r), subscriptNum q]
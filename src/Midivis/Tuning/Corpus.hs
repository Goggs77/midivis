{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Midivis.Tuning.Corpus where
import Data.Vector.Storable hiding (filter)
import Data.Vector.Algorithms
import Data.Vector.Algorithms.Intro
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Prelude (Double, Int, Bool(..), Ordering(..), Maybe(..), abs, error, flip, negate, mod, fromIntegral, floor, otherwise, (!!), (.), ($), (*), (+), (-), (/), (<=), (==), (/=), (>=), (>), (<), (&&))
import qualified Prelude as P
import Midivis.Util.Math
import Control.DeepSeq (NFData, deepseq)

forceTop :: (NFData a) => a -> a
forceTop x = x `deepseq` x
-- | Convention: [0, tone2/1, tone3/1, tone4/1,...] in log2 scale
--   so this type stores exponents of 2, which stays linear and relative
--   in terms of chord analysis
type Poly = Vector Double

unit :: Poly
unit = fromList [0.0]
-- | S for σ
type S² = Double

for :: (Storable a, Storable b) => Vector a -> (a -> b) -> Vector b
for = flip map

-- | Stack a interval(pl2) above another interval(pl1). It's operator is '+>'
stack :: Poly -> Poly -> Poly
stack pl1 pl2 = take (length pl1 - 1) pl1 ++ map (+(last pl1)) pl2

-- | Stack the second interval above the first interval, left-associative.
infixl 4 +>
(+>) :: Poly -> Poly -> Poly
(+>) = stack

-- | Repeatedly stack a given interval given times.
--   Extending it to negative numbers will result the same, just to save a 'inception'
infixl 5 *>
(*>) :: Poly -> Int -> Poly
(*>) _  0 = unit
(*>) pl 1 = pl
(*>) pl a = if a >= 0 then pl +> pl *> a - 1 else pl *> (negate a)

-- | Get an upside-down inversion of given interval, it's conjugate is '<<'
infixl 3 >>
(>>) :: Poly -> Int -> Poly
(>>) pl a' = let a = a' `mod` (length pl) in inception $ drop a pl ++ map (+ 1) (take a pl)

-- | Get an downside-up inversion of given interval, it's conjugate is '>>'
infixl 3 <<
(<<) :: Poly -> Int -> Poly
(<<) pl a = (>>) pl (-a)

-- | Difference tolerance: 1 cent (1/1200 octave). Below the practical
--   discrimination threshold of most listeners; near the trained limit.
fepsilog :: Double
fepsilog = 1.0 / 1200.0

-- | Approximately equal. Note that this operator does not apply log2 directly.
(~~) :: Double -> Double -> Bool
(~~) a b = abs (a - b) <= fepsilog

(>~) :: Double -> Double -> Bool
(>~) a b = a - b > fepsilog

(~<) :: Double -> Double -> Bool
(~<) a b = b - a > fepsilog

-- | APproximately comPARE, not idols.
appare :: Double -> Double -> Ordering
appare x y
  | x ~~ y = EQ
  | x >~ y = GT
  | x ~< y = LT
  | otherwise = error "The impossible happened"

mod1 :: Double -> Double
mod1 x = x - (fromIntegral $ floor x)

inception :: Vector Double -> Vector Double
inception v = map (+ (-v ! 0)) v

pack :: (Storable a) => [a] -> Vector a
pack = fromList

-- | Normalize a poly into one octave, relative to its lowest pitch:
--   sort ascending, shift so the lowest pitch is 0, reduce mod 1, dedupe.
--   Unlike 'fromFreqs' this takes log2 values already (no frequency
--   conversion). Use it before 'bestFit'/'polyName' for inputs built with
--   'scale12TET' or '+>' that may span octaves (e.g. maj7add13 = [0,4,7,11,21]).
octaveFold :: Poly -> Poly
octaveFold = nubBy appare . modify sort . map mod1 . inception . modify sort

-- | Single-note octave doublings of a poly: for each note, one copy of the
--   whole chord with that note repeated one octave up (unfolded, sorted).
--   Variants equal to the parent are dropped (e.g. a chord already
--   containing the doubled pitch class). e.g. maj9 [0,4,7,11,14] yields
--   [0,4,7,11,14,16] (E5), [0,4,7,11,14,19] (G5), ... in log2 scale.
octaveDoublings :: Poly -> [Poly]
octaveDoublings p =
  [ v
  | x <- toList p
  , let v = nubBy appare $ modify sort (p ++ singleton (x + 1.0))
  , v /= p
  ]

-- | Takes raw frequencies, outputs (processed Poly)
--   no identicals, strictly ascent order, cumulative
fromFreqs :: Vector Double -> Poly
fromFreqs fs = octaveFold $ map (log2) fs -- octaves become (mod 1)
-- using (log2 . from12TET) == (/12.0)

scale12TET :: [Double] -> Poly
scale12TET = map (/ 12) . pack


------------------------------------------------------------------------
-- 12-TET scale / chord corpus

-- | 1, 2, 3, 4, 5, 6, 7 --reference
ionian :: Poly
ionian = scale12TET [0, 2, 4, 5, 7, 9, 11]

-- | Alias of 'ionian'
major :: Poly
major = ionian

-- | 1, 2, ♭3, 4, 5, ♭6, ♭7
aeolian :: Poly
aeolian = scale12TET [0, 2, 3, 5, 7, 8, 10]

-- debug_ :: Vector (Double)
-- debug_ = map ((*440.0) . (2.0**)) aeolian

-- | Alias of 'aeolian'
minor :: Poly
minor = aeolian

-- | 1, 2, ♭3, 4, 5, ♭6, 7. or Aeolian ♮7
harmonicMinor :: Vector Double
harmonicMinor = scale12TET [0, 2, 3, 5, 7, 8, 11]

-- | 1, 2, ♭3, 4, 5, 6, ♭7
dorian :: Poly
dorian = scale12TET [0, 2, 3, 5, 7, 9, 10]

-- | 1, 2, ♭3, ♯4, 5, 6, ♭7
dorianSharp4 :: Poly
dorianSharp4 = scale12TET [0, 2, 3, 6, 7, 9, 10]

-- | 1, ♭2, ♭3, 4, 5, ♭6, ♭7
phrygian :: Poly
phrygian = scale12TET [0, 1, 3, 5, 7, 8, 10]

-- | 1, ♭2, 3, 4, 5, ♭6, ♭7
phrygianDominant :: Poly
phrygianDominant = scale12TET [0, 1, 4, 5, 7, 8, 10]

-- | 1, 2, 3, ♯4, 5, 6, 7
lydian :: Poly
lydian = scale12TET [0, 2, 4, 6, 7, 9, 11]

-- | 1, 2, 3, 4, 5, 6, ♭7
mixolydian :: Poly
mixolydian = scale12TET [0, 2, 4, 5, 7, 9, 10]

-- | 1, ♭2, ♭3, 4, ♭5, ♭6, ♭7
locrian :: Poly
locrian = scale12TET [0, 1, 3, 5, 6, 8, 10]

-- | 1, ♭2, ♭3, ♭4, ♭5, ♭6, ♭7
superLocrian :: Poly
superLocrian = scale12TET [0, 1, 3, 4, 6, 8, 10]

-- | Alias for 'superLocrian'
altered :: Poly
altered = superLocrian

-- | Whole tone scale, essentially 6 EDO
wholeTone :: Poly
wholeTone = scale12TET [0, 2, 4, 6, 8, 10]

half :: Poly
half = scale12TET [0, 1]

whole :: Poly
whole = scale12TET [0, 2]

maj :: Poly
maj = scale12TET [0, 4]

min :: Poly
min = scale12TET [0, 3]

fourth :: Poly
fourth = scale12TET [0, 5]

fifth :: Poly
fifth = scale12TET [0, 7]

tritone :: Poly
tritone = scale12TET [0, 6]

majTriad :: Poly
majTriad = maj +> min

minTriad :: Poly
minTriad = min +> maj

augTriad :: Poly
augTriad = maj *> 2

dimTriad :: Poly
dimTriad = min *> 2

maj7 :: Poly
maj7 = maj +> min +> maj

m7 :: Poly
m7 = min +> maj +> min

dom7 :: Poly
dom7 = maj +> min +> min

dim7 :: Poly
dim7 = min *> 3

-- | Half-diminished seventh: 1, ♭3, ♭5, ♭7. Flat 5 is 6 semitones.
m7b5 :: Poly
m7b5 = min +> min +> maj

-- | Major sixth: 1, 3, 5, 6
maj6 :: Poly
maj6 = scale12TET [0, 4, 7, 9]

-- | Minor sixth: 1, ♭3, 5, 6
m6 :: Poly
m6 = scale12TET [0, 3, 7, 9]

-- | 6/9 (major sixth add ninth): 1, 3, 5, 6, 9
maj69 :: Poly
maj69 = scale12TET [0, 4, 7, 9, 14]

-- | m6/9 (minor sixth add ninth): 1, ♭3, 5, 6, 9
m69 :: Poly
m69 = scale12TET [0, 3, 7, 9, 14]

-- | Major sixth add eleventh: 1, 3, 5, 6, 11
maj6add11 :: Poly
maj6add11 = scale12TET [0, 4, 7, 9, 17]

-- | 6/9/11: 1, 3, 5, 6, 9, 11
maj69add11 :: Poly
maj69add11 = scale12TET [0, 4, 7, 9, 14, 17]

-- | m6/9/11: 1, ♭3, 5, 6, 9, 11
m69add11 :: Poly
m69add11 = scale12TET [0, 3, 7, 9, 14, 17]

-- | Suspended second: 1, 2, 5
sus2 :: Poly
sus2 = scale12TET [0, 2, 7]

-- | Suspended fourth: 1, 4, 5
sus4 :: Poly
sus4 = scale12TET [0, 5, 7]

-- | Dominant seventh suspended fourth: 1, 4, 5, ♭7
dom7sus4 :: Poly
dom7sus4 = scale12TET [0, 5, 7, 10]

-- | Major ninth: 1, 3, 5, 7, 9 (maj7add9 shares this pitch set)
maj9 :: Poly
maj9 = scale12TET [0, 4, 7, 11, 14]

-- | Minor ninth: 1, ♭3, 5, ♭7, 9
m9 :: Poly
m9 = scale12TET [0, 3, 7, 10, 14]

-- | Dominant ninth: 1, 3, 5, ♭7, 9
dom9 :: Poly
dom9 = scale12TET [0, 4, 7, 10, 14]

-- | Major eleventh: 1, 3, 5, 7, 9, 11.
--   maj7 +> min alone gives [0,4,7,11,14] (a maj9); the 11th (17
--   semitones) needs a second stacked third.
maj11 :: Poly
maj11 = scale12TET [0, 4, 7, 11, 14, 17]

-- | Minor eleventh: 1, ♭3, 5, ♭7, 9, 11
m11 :: Poly
m11 = scale12TET [0, 3, 7, 10, 14, 17]

-- | Dominant eleventh: 1, 3, 5, ♭7, 9, 11
dom11 :: Poly
dom11 = scale12TET [0, 4, 7, 10, 14, 17]

-- | Major triad add ninth: 1, 3, 5, 9
majTriadAdd9 :: Poly
majTriadAdd9 = scale12TET [0, 4, 7, 14]

-- | Major triad add eleventh: 1, 3, 5, 11.
--   The 11th is 17 semitones (octave + 4), not 14 (that is the 9th).
majTriadAdd11 :: Poly
majTriadAdd11 = scale12TET [0, 4, 7, 17]

-- | Major seventh add eleventh: 1, 3, 5, 7, 11
maj7add11 :: Poly
maj7add11 = scale12TET [0, 4, 7, 11, 17]

-- | Major triad add thirteenth: 1, 3, 5, 13
majTriadAdd13 :: Poly
majTriadAdd13 = scale12TET [0, 4, 7, 21]

-- | Major triad add 9 add 11: 1, 3, 5, 9, 11
majTriadAdd9Add11 :: Poly
majTriadAdd9Add11 = scale12TET [0, 4, 7, 14, 17]

-- | Major triad add 9 add 13: 1, 3, 5, 9, 13
majTriadAdd9Add13 :: Poly
majTriadAdd9Add13 = scale12TET [0, 4, 7, 14, 21]

-- | Major triad add 11 add 13: 1, 3, 5, 11, 13
majTriadAdd11Add13 :: Poly
majTriadAdd11Add13 = scale12TET [0, 4, 7, 17, 21]

-- | Major triad add 9 add 11 add 13: 1, 3, 5, 9, 11, 13
majTriadAdd9Add11Add13 :: Poly
majTriadAdd9Add11Add13 = scale12TET [0, 4, 7, 14, 17, 21]

-- | Minor triad add ninth: 1, ♭3, 5, 9
minTriadAdd9 :: Poly
minTriadAdd9 = scale12TET [0, 3, 7, 14]

-- | Minor triad add eleventh: 1, ♭3, 5, 11
minTriadAdd11 :: Poly
minTriadAdd11 = scale12TET [0, 3, 7, 17]

-- | Minor triad add thirteenth: 1, ♭3, 5, 13
minTriadAdd13 :: Poly
minTriadAdd13 = scale12TET [0, 3, 7, 21]

-- | Minor triad add 9 add 11: 1, ♭3, 5, 9, 11
minTriadAdd9Add11 :: Poly
minTriadAdd9Add11 = scale12TET [0, 3, 7, 14, 17]

-- | Minor triad add 9 add 13: 1, ♭3, 5, 9, 13
minTriadAdd9Add13 :: Poly
minTriadAdd9Add13 = scale12TET [0, 3, 7, 14, 21]

-- | Minor triad add 11 add 13: 1, ♭3, 5, 11, 13
minTriadAdd11Add13 :: Poly
minTriadAdd11Add13 = scale12TET [0, 3, 7, 17, 21]

-- | Minor triad add 9 add 11 add 13: 1, ♭3, 5, 9, 11, 13
minTriadAdd9Add11Add13 :: Poly
minTriadAdd9Add11Add13 = scale12TET [0, 3, 7, 14, 17, 21]

-- | Minor sixth add eleventh: 1, ♭3, 5, 6, 11
m6add11 :: Poly
m6add11 = scale12TET [0, 3, 7, 9, 17]

-- | Minor seventh add eleventh: 1, ♭3, 5, ♭7, 11
m7add11 :: Poly
m7add11 = scale12TET [0, 3, 7, 10, 17]

-- | Major seventh add 13: 1, 3, 5, 7, 13. The 13th spans the octave
--   (21 semitones); keep it unfolded so direct bestFit distinguishes it
--   from the octave-reduced maj7(+6) pitch-class set.
maj7add13 :: Poly
maj7add13 = scale12TET [0, 4, 7, 11, 21]

-- | Major thirteenth: 1, 3, 5, 7, 9, 11, 13, spanning two octaves
--   (21 semitones on top of 0). Unfolded for the same reason as 'maj7add13'.
maj13 :: Poly
maj13 = scale12TET [0, 4, 7, 11, 14, 17, 21]

-- | Minor thirteenth: 1, ♭3, 5, ♭7, 9, 11, 13
m13 :: Poly
m13 = scale12TET [0, 3, 7, 10, 14, 17, 21]

-- | Dominant thirteenth: 1, 3, 5, ♭7, 9, 11, 13
dom13 :: Poly
dom13 = scale12TET [0, 4, 7, 10, 14, 17, 21]

all12TET :: [Poly]
all12TET =
  [ ionian,
    aeolian,
    harmonicMinor,
    dorian,
    dorianSharp4,
    phrygian,
    phrygianDominant,
    lydian,
    mixolydian,
    locrian,
    superLocrian,
    wholeTone,
    half,
    whole,
    maj,
    min,
    fourth,
    fifth,
    tritone,
    majTriad,
    minTriad,
    augTriad,
    dimTriad,
    maj7,
    m7,
    m7b5,
    dim7,
    dom7,
    maj6,
    m6,
    maj69,
    m69,
    maj6add11,
    maj69add11,
    m69add11,
    sus2,
    sus4,
    dom7sus4,
    maj9,
    m9,
    dom9,
    maj11,
    m11,
    dom11,
    majTriadAdd9,
    majTriadAdd11,
    maj7add11,
    majTriadAdd13,
    majTriadAdd9Add11,
    majTriadAdd9Add13,
    majTriadAdd11Add13,
    majTriadAdd9Add11Add13,
    minTriadAdd9,
    minTriadAdd11,
    minTriadAdd13,
    minTriadAdd9Add11,
    minTriadAdd9Add13,
    minTriadAdd11Add13,
    minTriadAdd9Add11Add13,
    m6add11,
    m7add11,
    maj7add13,
    maj13,
    m13,
    dom13,
    octave
  ]

scaleRational :: [Int] -> Poly
scaleRational rs = fromFreqs $ map ((/ (fromIntegral $ rs !! 0)) . fromIntegral) $ pack rs

-- | Build an UNFOLDED poly from integer frequency ratios.
--   Like 'scaleRational' (ratios relative to the lowest note) but does NOT
--   octave-fold or dedupe: the top note may lie above the octave, matching
--   the unfolded 12-TET entries ('maj7add13', 'maj13'). Sort ascending,
--   then subtract the lowest in log2 space (an 'inception').
scaleRationalOpen :: [Int] -> Poly
scaleRationalOpen rs =
  let rsSorted = modify sort $ pack rs
      root     = fromIntegral (rsSorted ! 0)
  in inception $ map (log2 . (/ root) . fromIntegral) rsSorted


------------------------------------------------------------------------
-- Just intonation corpus, grouped by prime limit.
--   Naming follows the Xenharmonic Wiki gallery of just intervals.
--   Each group holds intervals whose ratio's LARGEST prime factor is
--   that limit, so groups are disjoint and 'concat' is lossless.

-- 3-limit (Pythagorean): {2, 3}
octave :: Poly
octave = scaleRational [1, 2]

-- | 4/3 just perfect fourth
perfect4 :: Poly
perfect4 = scaleRational [3, 4]

-- | 3/2 just perfect fifth
perfect5 :: Poly
perfect5 = scaleRational [2, 3]

-- | 9/8 Pythagorean whole tone
pythagoreanWholeTone :: Poly
pythagoreanWholeTone = scaleRational [8, 9]

-- | 32/27 Pythagorean minor third
pythagoreanMinor3 :: Poly
pythagoreanMinor3 = scaleRational [27, 32]

-- | 81/64 Pythagorean major third
pythagoreanMajor3 :: Poly
pythagoreanMajor3 = scaleRational [64, 81]

-- | 27/16 Pythagorean major sixth
pythagoreanMajor6 :: Poly
pythagoreanMajor6 = scaleRational [16, 27]

-- | 16/9 Pythagorean minor seventh
pythagoreanMin7 :: Poly
pythagoreanMin7 = scaleRational [9, 16]

-- | 243/128 Pythagorean major seventh
pythagoreanMajor7 :: Poly
pythagoreanMajor7 = scaleRational [128, 243]

-- | 256/243 Pythagorean limma (minor semitone)
pythagoreanLimma :: Poly
pythagoreanLimma = scaleRational [243, 256]

-- | 2187/2048 Pythagorean apotome (major semitone)
pythagoreanApotome :: Poly
pythagoreanApotome = scaleRational [2048, 2187]

-- | 729/512 Pythagorean tritone
pythagoreanTritone :: Poly
pythagoreanTritone = scaleRational [512, 729]

-- | 1024/729 Pythagorean diminished fifth
pythagoreanDim5 :: Poly
pythagoreanDim5 = scaleRational [729, 1024]

-- | Intervals whose largest prime factor is 3
allPythagorean :: [Poly]
allPythagorean =
  [ octave,
    perfect4,
    perfect5,
    pythagoreanWholeTone,
    pythagoreanMinor3,
    pythagoreanMajor3,
    pythagoreanMajor6,
    pythagoreanMin7,
    pythagoreanMajor7,
    pythagoreanLimma,
    pythagoreanApotome,
    pythagoreanTritone,
    pythagoreanDim5
  ]

-- 5-limit (classic / just): {2, 3, 5}
-- | 5/4 just major third
justMaj3 :: Poly
justMaj3 = scaleRational [4, 5]

-- | 6/5 just minor third
justMin3 :: Poly
justMin3 = scaleRational [5, 6]

-- | 8/5 just minor sixth
justMin6 :: Poly
justMin6 = scaleRational [5, 8]

-- | 5/3 just major sixth
justMaj6 :: Poly
justMaj6 = scaleRational [3, 5]

-- | 15/8 just major seventh
justMaj7 :: Poly
justMaj7 = scaleRational [8, 15]

-- | 9/5 large minor seventh
largeMin7 :: Poly
largeMin7 = scaleRational [5, 9]

-- | 10/9 classic minor whole tone
justMinorWholeTone :: Poly
justMinorWholeTone = scaleRational [9, 10]

-- | 16/15 classic diatonic semitone
justSemitone :: Poly
justSemitone = scaleRational [15, 16]

-- | 25/24 classic chromatic semitone (chroma)
justChroma :: Poly
justChroma = scaleRational [24, 25]

-- | 32/25 classic diminished fourth
justDim4 :: Poly
justDim4 = scaleRational [25, 32]

-- | 25/16 classic augmented fifth
justAug5 :: Poly
justAug5 = scaleRational [16, 25]

-- | 36/25 classic diminished fifth
justDim5 :: Poly
justDim5 = scaleRational [25, 36]

-- | 45/32 diatonic tritone
justTritone :: Poly
justTritone = scaleRational [32, 45]

-- | 40/27 grave fifth
graveFifth :: Poly
graveFifth = scaleRational [27, 40]

-- | Intervals whose largest prime factor is 5
allClassic :: [Poly]
allClassic =
  [ justMaj3,
    justMin3,
    justMin6,
    justMaj6,
    justMaj7,
    largeMin7,
    justMinorWholeTone,
    justSemitone,
    justChroma,
    justDim4,
    justAug5,
    justDim5,
    justTritone,
    graveFifth
  ]

-- 7-limit (septimal): {2, 3, 5, 7}
-- | 7/6 subminor third
subMin3 :: Poly
subMin3 = scaleRational [6, 7]

-- | 14/9 subminor sixth
subMin6 :: Poly
subMin6 = scaleRational [9, 14]

-- | 7/4 subminor seventh (harmonic seventh)
subMin7 :: Poly
subMin7 = scaleRational [4, 7]

-- | 8/7 supermajor second (septimal whole tone)
superMaj2 :: Poly
superMaj2 = scaleRational [7, 8]

-- | 9/7 supermajor third (septimal major third)
superMaj3 :: Poly
superMaj3 = scaleRational [7, 9]

-- | 12/7 supermajor sixth (septimal major sixth)
superMaj6 :: Poly
superMaj6 = scaleRational [7, 12]

-- | 7/5 augmented fourth (septimal tritone)
aug4 :: Poly
aug4 = scaleRational [5, 7]

-- | 10/7 diminished fifth (Euler's tritone)
dim4 :: Poly
dim4 = scaleRational [7, 10]

-- | 36/35 septimal quarter tone
septimalQuarterTone :: Poly
septimalQuarterTone = scaleRational [35, 36]

-- | 28/27 septimal third tone (subminor second)
septimalThirdTone :: Poly
septimalThirdTone = scaleRational [27, 28]

-- | 21/20 septimal chromatic semitone
septimalChromaticSemitone :: Poly
septimalChromaticSemitone = scaleRational [20, 21]

-- | 15/14 septimal diatonic semitone
septimalDiatonicSemitone :: Poly
septimalDiatonicSemitone = scaleRational [14, 15]

-- | 35/32 septimal neutral second
septimalNeutral2 :: Poly
septimalNeutral2 = scaleRational [32, 35]

-- | 60/49 smaller septimal neutral third
smallerSeptimalNeutral3 :: Poly
smallerSeptimalNeutral3 = scaleRational [49, 60]

-- | 21/16 sub-fourth (narrow fourth)
sub4 :: Poly
sub4 = scaleRational [16, 21]

-- | 48/35 septimal super-fourth
septimalSuper4 :: Poly
septimalSuper4 = scaleRational [35, 48]

-- | 32/21 super-fifth (wide fifth)
super5 :: Poly
super5 = scaleRational [21, 32]

-- | 63/32 septimal suboctave
septimalSuboctave :: Poly
septimalSuboctave = scaleRational [32, 63]

-- | Intervals whose largest prime factor is 7
allSeptimal :: [Poly]
allSeptimal =
  [ subMin3,
    subMin6,
    subMin7,
    superMaj2,
    superMaj3,
    superMaj6,
    aug4,
    dim4,
    septimalQuarterTone,
    septimalThirdTone,
    septimalChromaticSemitone,
    septimalDiatonicSemitone,
    septimalNeutral2,
    smallerSeptimalNeutral3,
    sub4,
    septimalSuper4,
    super5,
    septimalSuboctave
  ]

-- 11-limit (undecimal): {2, 3, 5, 7, 11}
-- | 11/9 undecimal neutral third
undecimalNeutral3 :: Poly
undecimalNeutral3 = scaleRational [9, 11]

-- | 18/11 undecimal neutral sixth
undecimalNeutral6 :: Poly
undecimalNeutral6 = scaleRational [11, 18]

-- | 11/7 undecimal subminor sixth
undecimalSubmin6 :: Poly
undecimalSubmin6 = scaleRational [7, 11]

-- | 11/8 super-fourth (undecimal semi-augmented fourth)
super4 :: Poly
super4 = scaleRational [8, 11]

-- | 33/32 undecimal quarter tone
undecimalQuarterTone :: Poly
undecimalQuarterTone = scaleRational [32, 33]

-- | 11/10 undecimal neutral second (Ptolemy's second)
undecimalNeutral2 :: Poly
undecimalNeutral2 = scaleRational [10, 11]

-- | 14/11 undecimal major third
undecimalMajor3 :: Poly
undecimalMajor3 = scaleRational [11, 14]

-- | 15/11 undecimal augmented fourth
undecimalAug4 :: Poly
undecimalAug4 = scaleRational [11, 15]

-- | 22/15 undecimal diminished fifth
undecimalDim5 :: Poly
undecimalDim5 = scaleRational [15, 22]

-- | 16/11 undecimal sub-fifth
undecimalSub5 :: Poly
undecimalSub5 = scaleRational [11, 16]

-- | 11/6 undecimal neutral seventh
undecimalNeutral7 :: Poly
undecimalNeutral7 = scaleRational [6, 11]

-- | 21/11 undecimal major seventh
undecimalMajor7 :: Poly
undecimalMajor7 = scaleRational [11, 21]

-- | Intervals whose largest prime factor is 11
allUndecimal :: [Poly]
allUndecimal =
  [ undecimalNeutral3,
    undecimalNeutral6,
    undecimalSubmin6,
    super4,
    undecimalQuarterTone,
    undecimalNeutral2,
    undecimalMajor3,
    undecimalAug4,
    undecimalDim5,
    undecimalSub5,
    undecimalNeutral7,
    undecimalMajor7
  ]

-- 13-limit (tridecimal): {2, 3, 5, 7, 11, 13}
-- | 13/9 tridecimal diminished fifth
tridecimalDim5 :: Poly
tridecimalDim5 = scaleRational [9, 13]

-- | 13/8 lesser tridecimal neutral sixth
lesserTridecimalNeutral6 :: Poly
lesserTridecimalNeutral6 = scaleRational [8, 13]

-- | 13/7 tridecimal submajor seventh
tridecimalSubmaj7 :: Poly
tridecimalSubmaj7 = scaleRational [7, 13]

-- | 13/12 tridecimal neutral second
tridecimalNeutral2 :: Poly
tridecimalNeutral2 = scaleRational [12, 13]

-- | 13/11 tridecimal minor third
tridecimalMinor3 :: Poly
tridecimalMinor3 = scaleRational [11, 13]

-- | 26/21 tridecimal submajor third
tridecimalSubmaj3 :: Poly
tridecimalSubmaj3 = scaleRational [21, 26]

-- | 16/13 greater tridecimal neutral third
greaterTridecimalNeutral3 :: Poly
greaterTridecimalNeutral3 = scaleRational [13, 16]

-- | 18/13 tridecimal augmented fourth
tridecimalAug4 :: Poly
tridecimalAug4 = scaleRational [13, 18]

-- | 26/17 tridecimal super-fifth
tridecimalSuper5 :: Poly
tridecimalSuper5 = scaleRational [17, 26]

-- | 21/13 tridecimal supraminor sixth
tridecimalSupraminor6 :: Poly
tridecimalSupraminor6 = scaleRational [13, 21]

-- | 24/13 tridecimal neutral seventh
tridecimalNeutral7 :: Poly
tridecimalNeutral7 = scaleRational [13, 24]

-- | Intervals whose largest prime factor is 13
allTridecimal :: [Poly]
allTridecimal =
  [ tridecimalDim5,
    lesserTridecimalNeutral6,
    tridecimalSubmaj7,
    tridecimalNeutral2,
    tridecimalMinor3,
    tridecimalSubmaj3,
    greaterTridecimalNeutral3,
    tridecimalAug4,
    tridecimalSuper5,
    tridecimalSupraminor6,
    tridecimalNeutral7
  ]

-- 17-limit (septendecimal): {2, 3, 5, 7, 11, 13, 17}
-- | 17/16 septendecimal semitone
septendecimalSemitone :: Poly
septendecimalSemitone = scaleRational [16, 17]

-- | 17/15 septendecimal whole tone
septendecimalWholeTone :: Poly
septendecimalWholeTone = scaleRational [15, 17]

-- | 17/13 septendecimal sub-fourth
septendecimalSub4 :: Poly
septendecimalSub4 = scaleRational [13, 17]

-- | 17/12 larger septendecimal tritone
septendecimalTritone :: Poly
septendecimalTritone = scaleRational [12, 17]

-- | 17/11 septendecimal subminor sixth
septendecimalSubmin6 :: Poly
septendecimalSubmin6 = scaleRational [11, 17]

-- | 17/10 septendecimal diminished seventh
septendecimalDim7 :: Poly
septendecimalDim7 = scaleRational [10, 17]

-- | 30/17 septendecimal minor seventh
septendecimalMin7 :: Poly
septendecimalMin7 = scaleRational [17, 30]

-- | Intervals whose largest prime factor is 17
allSeptendecimal :: [Poly]
allSeptendecimal =
  [ septendecimalSemitone,
    septendecimalWholeTone,
    septendecimalSub4,
    septendecimalTritone,
    septendecimalSubmin6,
    septendecimalDim7,
    septendecimalMin7
  ]

-- 19-limit (undevicesimal): {2, 3, 5, 7, 11, 13, 17, 19}
-- | 20/19 undevicesimal semitone
undevicesimalSemitone :: Poly
undevicesimalSemitone = scaleRational [19, 20]

-- | 19/16 undevicesimal minor third
undevicesimalMinor3 :: Poly
undevicesimalMinor3 = scaleRational [16, 19]

-- | 24/19 undevicesimal major third
undevicesimalMajor3 :: Poly
undevicesimalMajor3 = scaleRational [19, 24]

-- | 27/19 undevicesimal tritone
undevicesimalTritone :: Poly
undevicesimalTritone = scaleRational [19, 27]

-- | 32/19 undevicesimal major sixth
undevicesimalMajor6 :: Poly
undevicesimalMajor6 = scaleRational [19, 32]

-- | Intervals whose largest prime factor is 19
allUndevicesimal :: [Poly]
allUndevicesimal =
  [ undevicesimalSemitone,
    undevicesimalMinor3,
    undevicesimalMajor3,
    undevicesimalTritone,
    undevicesimalMajor6
  ]

-- 23-limit (vicesimotertial): {2, 3, 5, 7, 11, 13, 17, 19, 23}
-- | 24/23 vicesimotertial semitone
vicesimotertialSemitone :: Poly
vicesimotertialSemitone = scaleRational [23, 24]

-- | 28/23 vicesimotertial neutral third
vicesimotertialNeutral3 :: Poly
vicesimotertialNeutral3 = scaleRational [23, 28]

-- | 23/16 vicesimotertial superaugmented fourth
vicesimotertialSuper4 :: Poly
vicesimotertialSuper4 = scaleRational [16, 23]

-- | Intervals whose largest prime factor is 23
allVicesimotertial :: [Poly]
allVicesimotertial =
  [ vicesimotertialSemitone,
    vicesimotertialNeutral3,
    vicesimotertialSuper4
  ]

-- >= 29-limit: octave-reduced harmonics and notable tempering intervals
-- | 29/16 octave-reduced 29th harmonic (vicesimononal supraminor seventh)
harmonic29 :: Poly
harmonic29 = scaleRational [16, 29]

-- | 31/16 octave-reduced 31st harmonic (tricesimoprimal ultramajor seventh)
harmonic31 :: Poly
harmonic31 = scaleRational [16, 31]

-- | 41/32 octave-reduced 41st harmonic (quadracesimoprimal supermajor third)
harmonic41 :: Poly
harmonic41 = scaleRational [32, 41]

-- | 61/32 octave-reduced 61st harmonic
harmonic61 :: Poly
harmonic61 = scaleRational [32, 61]

-- | 97/64 octave-reduced 97th harmonic (nonacesimoseptimal harmonic fifth)
harmonic97 :: Poly
harmonic97 = scaleRational [64, 97]

-- | Intervals whose largest prime factor is >= 29
--   (63/32 is NOT here: 63 = 7*9, largest prime factor 7, so it lives in
--   the 7-limit group as 'septimalSuboctave'.)
allHigherLimit :: [Poly]
allHigherLimit =
  [ harmonic29,
    harmonic31,
    harmonic41,
    harmonic61,
    harmonic97
  ]

-- | Every just interval above, octave-reduced, in log2 scale.
--   Grouped by prime limit; the union covers all entries.
allJustIntonation :: [Poly]
allJustIntonation = P.concat
  [ allPythagorean,
    allClassic,
    allSeptimal,
    allUndecimal,
    allTridecimal,
    allSeptendecimal,
    allUndevicesimal,
    allVicesimotertial,
    allHigherLimit
  ]


------------------------------------------------------------------------
-- Just intonation chords, unfolded (top note may exceed the octave).
--   Source: endolith's gist 3098720 "List of chords in just intonation",
--   with corrections: 54:64:243 -> 54:64:81 (Pythagorean minor triad),
--   64:81:96 relabeled Pythagorean major triad, 4:5:6:18 removed (its
--   pitch-class set duplicates the mu chord), 8:10:12:15 named simply
--   "major seventh". Doubtful entries are kept with a comment.

-- | Power chord / fifth chord (2:3:4)
powerFifth :: Poly
powerFifth = scaleRationalOpen [2, 3, 4]

-- | Bohlen-Pierce major triad (3:5:7)
bpMajorTriad :: Poly
bpMajorTriad = scaleRationalOpen [3, 5, 7]

-- | Just major triad (4:5:6)
justMajorTriad :: Poly
justMajorTriad = scaleRationalOpen [4, 5, 6]

-- | Perfect diminished chord (5:6:7) -- non-standard name, kept from gist
perfectDimTriad :: Poly
perfectDimTriad = scaleRationalOpen [5, 6, 7]

-- | Harmonic-series minor triad (6:7:9) -- subminor third 7/6
septimalMinorTriad :: Poly
septimalMinorTriad = scaleRationalOpen [6, 7, 9]

-- | Just suspended fourth (6:8:9)
justSus4 :: Poly
justSus4 = scaleRationalOpen [6, 8, 9]

-- | Just suspended second (8:9:12)
justSus2 :: Poly
justSus2 = scaleRationalOpen [8, 9, 12]

-- | Just minor triad, first minor triad in the harmonic series (10:12:15)
justMinorTriad :: Poly
justMinorTriad = scaleRationalOpen [10, 12, 15]

-- | False minor triad (16:19:24) -- gist author's own "?", kept with doubt
falseMinorTriad :: Poly
falseMinorTriad = scaleRationalOpen [16, 19, 24]

-- | Just augmented triad (16:20:25)
justAugTriad :: Poly
justAugTriad = scaleRationalOpen [16, 20, 25]

-- | Supertonic triad (27:32:40) -- fifth is the wolf 40/27; kept with doubt
supertonicTriad :: Poly
supertonicTriad = scaleRationalOpen [27, 32, 40]

-- | Just diminished triad (45:54:64)
justDimTriad :: Poly
justDimTriad = scaleRationalOpen [45, 54, 64]

-- | Pythagorean minor triad (54:64:81) -- corrected from gist's 54:64:243
pythagoreanMinorTriad :: Poly
pythagoreanMinorTriad = scaleRationalOpen [54, 64, 81]

-- | Pythagorean major triad (64:81:96) -- relabeled from gist's "minor"
pythagoreanMajorTriad :: Poly
pythagoreanMajorTriad = scaleRationalOpen [64, 81, 96]

-- | Bohlen-Pierce primary tetrad (3:5:7:9)
bpTetrad :: Poly
bpTetrad = scaleRationalOpen [3, 5, 7, 9]

-- | Anomalous saturated suspension (3:5:9:15) (x31eq)
ass15 :: Poly
ass15 = scaleRationalOpen [3, 5, 9, 15]

-- | Anomalous saturated suspension (3:7:9:21)
ass21 :: Poly
ass21 = scaleRationalOpen [3, 7, 9, 21]

-- | Harmonic seventh chord (4:5:6:7)
harmonicSeventh :: Poly
harmonicSeventh = scaleRationalOpen [4, 5, 6, 7]

-- | Half-diminished seventh, 7-limit (5:6:7:9)
justHalfDim7 :: Poly
justHalfDim7 = scaleRationalOpen [5, 6, 7, 9]

-- | Mu major chord (8:9:10:12)
muChord :: Poly
muChord = scaleRationalOpen [8, 9, 10, 12]

-- | Major seventh (8:10:12:15) -- gist said "major/major"; plain name used
justMajorSeventh :: Poly
justMajorSeventh = scaleRationalOpen [8, 10, 12, 15]

-- | Minor seventh (10:12:15:18)
justMinorSeventh :: Poly
justMinorSeventh = scaleRationalOpen [10, 12, 15, 18]

-- | Dream chord (12:16:17:18)
dreamChord :: Poly
dreamChord = scaleRationalOpen [12, 16, 17, 18]

-- | Dominant seventh (20:25:30:36)
justDom7 :: Poly
justDom7 = scaleRationalOpen [20, 25, 30, 36]

-- | Half-diminished seventh, 5-limit (25:30:36:45)
classicHalfDim7 :: Poly
classicHalfDim7 = scaleRationalOpen [25, 30, 36, 45]

-- | Minor/major seventh (40:48:60:75)
mMaj7 :: Poly
mMaj7 = scaleRationalOpen [40, 48, 60, 75]

-- | Augmented seventh (80:100:125:144)
justAug7 :: Poly
justAug7 = scaleRationalOpen [80, 100, 125, 144]

-- | Dominant ninth (4:5:6:7:9) -- 9/4 = 2.25 unfolded (log2 ~ 1.1699)
justDom9 :: Poly
justDom9 = scaleRationalOpen [4, 5, 6, 7, 9]

-- | Dominant minor ninth (8:10:12:14:17) -- 17/8 unfolded (log2 ~ 1.0875)
justDomMin9 :: Poly
justDomMin9 = scaleRationalOpen [8, 10, 12, 14, 17]

-- | Dominant seventh sharp ninth (20:25:30:36:48) -- 12/5 unfolded
justDom7sharp9 :: Poly
justDom7sharp9 = scaleRationalOpen [20, 25, 30, 36, 48]

-- | Just intonation triads
allJustTriads :: [Poly]
allJustTriads =
  [ powerFifth,
    bpMajorTriad,
    justMajorTriad,
    perfectDimTriad,
    septimalMinorTriad,
    justSus4,
    justSus2,
    justMinorTriad,
    falseMinorTriad,
    justAugTriad,
    supertonicTriad,
    justDimTriad,
    pythagoreanMinorTriad,
    pythagoreanMajorTriad
  ]

-- | Just intonation seventh chords (tetrads)
allJustSevenths :: [Poly]
allJustSevenths =
  [ bpTetrad,
    ass15,
    ass21,
    harmonicSeventh,
    justHalfDim7,
    muChord,
    justMajorSeventh,
    justMinorSeventh,
    dreamChord,
    justDom7,
    classicHalfDim7,
    mMaj7,
    justAug7
  ]

-- | Just intonation ninth chords (pentads)
allJustPentads :: [Poly]
allJustPentads =
  [ justDom9,
    justDomMin9,
    justDom7sharp9
  ]

-- | Every just intonation chord above, unfolded, in log2 scale.
allJustChords :: [Poly]
allJustChords = P.concat [allJustTriads, allJustSevenths, allJustPentads]

-- | 12-TET chord name entries (the chord half of the 12-TET polyNames
--   section). Shared by 'polyNames' and the octave-doubling variants, so
--   variants always display exactly like their parent (no self-reference).
tetChordNames :: [(Poly, T.Text)]
tetChordNames =
  [ (majTriad,          "major triad ()")
  , (minTriad,          "minor triad (m)")
  , (augTriad,          "augmented triad (aug)")
  , (dimTriad,          "diminished triad (dim)")
  , (maj7,              "major seventh chord (maj7)")
  , (m7,                "minor seventh chord (m7)")
  , (dom7,              "dominant seventh chord (7)")
  , (dim7,              "diminished seventh chord (dim7)")
  , (m7b5,              "half-diminished seventh chord (m7b5)")
  , (maj6,              "major sixth chord (maj6)")
  , (m6,                "minor sixth chord (m6)")
  , (maj69,             "6/9 chord (maj6\x2079)")
  , (m69,               "m6/9 chord (m6\x2079)")
  , (maj6add11,         "major sixth add eleventh (6add11)")
  , (maj69add11,        "6/9/11 chord (6/9/11)")
  , (m69add11,          "m6/9/11 chord (m6/9/11)")
  , (sus2,              "suspended second (sus2)")
  , (sus4,              "suspended fourth (sus4)")
  , (dom7sus4,          "dominant seventh suspended fourth (7sus4)")
  , (maj9,              "major ninth (maj9)")
  , (m9,                "minor ninth (m9)")
  , (dom9,              "dominant ninth (9)")
  , (maj11,             "major eleventh (maj11)")
  , (m11,               "minor eleventh (m11)")
  , (dom11,             "dominant eleventh (11)")
  , (majTriadAdd9,      "major triad add ninth (add9)")
  , (majTriadAdd11,     "major triad add eleventh (add11)")
  , (maj7add11,         "major seventh add eleventh (maj7add11)")
  , (majTriadAdd13,     "major triad add thirteenth (add13)")
  , (majTriadAdd9Add11, "major triad add 9, 11 (maj11no7)")
  , (majTriadAdd9Add13, "major triad add 9, 13 (add9,13)")
  , (majTriadAdd11Add13, "major triad add 11, 13 (add11,13)")
  , (majTriadAdd9Add11Add13, "major triad add 9, 11, 13 (maj13no7)")
  , (minTriadAdd9,      "minor triad add ninth (madd9)")
  , (minTriadAdd11,     "minor triad add eleventh (madd11)")
  , (minTriadAdd13,     "minor triad add thirteenth (madd13)")
  , (minTriadAdd9Add11, "minor triad add 9, 11 (m11no7)")
  , (minTriadAdd9Add13, "minor triad add 9, 13 (madd9,13)")
  , (minTriadAdd11Add13, "minor triad add 11, 13 (madd11,13)")
  , (minTriadAdd9Add11Add13, "minor triad add 9, 11, 13 (m13no7)")
  , (m6add11,           "minor sixth add eleventh (m6add11)")
  , (m7add11,           "minor seventh add eleventh (m7add11)")
  , (maj7add13,         "major seventh add 13 (maj7add13)")
  , (maj13,             "major thirteenth (maj13)")
  , (m13,               "minor thirteenth (m13)")
  , (dom13,             "dominant thirteenth (13)")
  ]

-- | Just intonation chord name entries; shared the same way as 'tetChordNames'.
justChordNames :: [(Poly, T.Text)]
justChordNames =
  [ (powerFifth,           "power chord / fifth chord (2:3:4)")
  , (bpMajorTriad,         "Bohlen-Pierce major triad (3:5:7)")
  , (justMajorTriad,       "just major triad (4:5:6)")
  , (perfectDimTriad,      "perfect diminished chord (5:6:7)")
  , (septimalMinorTriad,   "harmonic-series minor triad (6:7:9)")
  , (justSus4,             "just suspended fourth (6:8:9)")
  , (justSus2,             "just suspended second (8:9:12)")
  , (justMinorTriad,       "just minor triad (10:12:15)")
  , (falseMinorTriad,      "false minor triad (16:19:24)")
  , (justAugTriad,         "just augmented triad (16:20:25)")
  , (supertonicTriad,      "supertonic triad (27:32:40)")
  , (justDimTriad,         "just diminished triad (45:54:64)")
  , (pythagoreanMinorTriad,"Pythagorean minor triad (54:64:81)")
  , (pythagoreanMajorTriad,"Pythagorean major triad (64:81:96)")
  , (bpTetrad,             "Bohlen-Pierce primary tetrad (3:5:7:9)")
  , (ass15,                "anomalous saturated suspension (3:5:9:15)")
  , (ass21,                "anomalous saturated suspension (3:7:9:21)")
  , (harmonicSeventh,      "harmonic seventh chord (4:5:6:7)")
  , (justHalfDim7,         "half-diminished seventh, 7-limit (5:6:7:9)")
  , (muChord,              "μ major chord (8:9:10:12)")
  , (justMajorSeventh,     "major seventh (8:10:12:15)")
  , (justMinorSeventh,     "minor seventh (10:12:15:18)")
  , (dreamChord,           "dream chord (12:16:17:18)")
  , (justDom7,             "dominant seventh (20:25:30:36)")
  , (classicHalfDim7,      "half-diminished seventh, 5-limit (25:30:36:45)")
  , (mMaj7,                "minor/major seventh (40:48:60:75)")
  , (justAug7,             "augmented seventh (80:100:125:144)")
  , (justDom9,             "dominant ninth (4:5:6:7:9)")
  , (justDomMin9,          "dominant minor ninth (8:10:12:14:17)")
  , (justDom7sharp9,       "dominant seventh sharp ninth (20:25:30:36:48)")
  ]

-- | Octave-doubling variants of every chord, carrying the parent's name.
doublingPairs :: [(Poly, T.Text)]
doublingPairs =
  [ (v, n) | (p, n) <- tetChordNames P.++ justChordNames, v <- octaveDoublings p ]

allDoublings :: [Poly]
allDoublings = P.map P.fst doublingPairs

-- | Precomputed matching corpus. Trading memory for time:
--   exact/near-exact hits (including *inversions*) skip the O(n) fit loop
--   entirely, and the fit path uses pre-materialized inversions plus an
--   allocation-free deviation metric instead of allocating vectors per
--   ('>>') call and per 'fit'.
data CorpusTable = CorpusTable
  { ctExact :: M.Map Poly T.Text                   -- ^ exact value -> name
  , ctByLen :: M.Map Int [Poly]                    -- ^ length -> candidates (root positions)
  , ctRot   :: M.Map Poly (Poly, Int)              -- ^ rotation value -> (candidate, inv)
  , ctRotByLen :: M.Map Int [(Poly, (Poly, Int))]  -- ^ length -> rotations (for 1-cent scan)
  , ctCandsByLen :: M.Map Int [(Poly, [(Poly, Int)])]  -- ^ length -> candidate w/ inversions
  }

-- | Build a matching table from a corpus list.
buildCorpusTable :: [Poly] -> CorpusTable
buildCorpusTable polys =
  CorpusTable
    { ctExact = M.fromList [ (p, n) | p <- polys, Just n <- [polyName p] ]
    , ctByLen = M.fromListWith (P.++) [ (length p, [p]) | p <- polys ]
    , ctRot = M.fromListWith preferRot
        [ (r, (p, i)) | p <- polys, (r, i) <- invs p ]
    , ctRotByLen = M.fromListWith consRev
        [ (length r, [(r, (p, i))]) | p <- polys, (r, i) <- invs p ]
    , ctCandsByLen = M.fromListWith consRev
        [ (length p, [(p, invs p)]) | p <- polys ]
    }
  where
    invs p = [ (p >> d, d) | d <- [0 .. length p - 1] ]
    -- corpus order preserved inside each bucket (later entries appended)
    consRev new old = old P.++ new
    -- rotation-value collisions (e.g. symmetric chords like dim7): prefer
    -- the smaller inversion, then the earlier corpus entry -- matches the
    -- 'betterFit' tie-break (equal lengths, tertiary = smallest inv).
    preferRot n o = if P.snd n < P.snd o then n else o

-- | The standard matching table over the full corpus.
allCorpusTable :: CorpusTable
allCorpusTable = buildCorpusTable allPoly

------------------------------------------------------------------------
-- ================================================================== --
allPoly :: [Poly]
allPoly = forceTop $ P.concat $ P.map (P.map forceTop) 
    [all12TET, allJustIntonation, allJustChords, allDoublings]
-- ================================================================== --
------------------------------------------------------------------------
-- Poly -> name lookup

-- | Near-exact poly equality tolerating double rounding noise.
--   Absolute tolerance 1.2e-15 (~5 ulp) replaces (==) in 'polyName',
--   so float drift from ('>>')/('<<') still matches the canonical value.
polyUlp :: Poly -> Poly -> Bool
polyUlp p1 p2 = length p1 == length p2 && and (zipWith (\ x y -> abs (x - y) <= 1.2e-15) p1 p2)

-- | Approximate equality: same length and pairwise ('~~').
--   Local copy of 'Midivis.Tuning.Poly.~='; Corpus cannot import Poly (cycle).
-- | Zero-allocation '~~' over two polys (same length, pairwise within
--   'fepsilog'). Hot path: 'bestFitTable' scans whole buckets with this.
polyApprox :: Poly -> Poly -> Bool
polyApprox p1 p2 = length p1 == length p2 && go 0
  where
    n = length p1
    go i
      | i >= n    = True
      | otherwise = abs (p1 ! i - p2 ! i) <= fepsilog && go (i + 1)

-- | Human-readable names for every distinct corpus poly.
--   One entry per distinct value: aliases (major=ionian, minor=aeolian,
--   altered=superLocrian) are merged, and 'octave' is listed once (12-TET).
--   Canonical inputs hit 'polyUlp' exactly; only inputs further than
--   1.2e-15 from every key fall to the 1-cent 'polyApprox' pass, where
--   earlier sections win (12-TET first).
polyNames :: [(Poly, T.Text)]
polyNames = forceTop $ P.concat $ P.map (P.map forceTop)
  [ -- 12-TET scales / chords
    [ (ionian,           "major scale (ionian)")
    , (aeolian,          "natural minor scale (aeolian)")
    , (harmonicMinor,    "harmonic minor scale")
    , (dorian,           "dorian scale")
    , (dorianSharp4,     "dorian sharp 4 scale")
    , (phrygian,         "phrygian scale")
    , (phrygianDominant, "phrygian dominant scale")
    , (lydian,           "lydian scale")
    , (mixolydian,       "mixolydian scale")
    , (locrian,          "locrian scale")
    , (superLocrian,     "altered scale (super locrian)") -- altered alias merged
    , (wholeTone,        "whole tone scale")
    , (half,             "half step")
    , (whole,            "whole step")
    , (maj,              "major third")
    , (min,              "minor third")
    , (fourth,           "perfect fourth")
    , (fifth,            "perfect fifth")
    , (tritone,          "tritone")
    , (octave,           "octave")
    ]
  , -- 12-TET chords
    tetChordNames
  , -- 3-limit (Pythagorean)
    [ (perfect4,             "just perfect fourth (4/3)")
    , (perfect5,             "just perfect fifth (3/2)")
    , (pythagoreanWholeTone, "Pythagorean whole tone (9/8)")
    , (pythagoreanMinor3,    "Pythagorean minor third (32/27)")
    , (pythagoreanMajor3,    "Pythagorean major third (81/64)")
    , (pythagoreanMajor6,    "Pythagorean major sixth (27/16)")
    , (pythagoreanMin7,      "Pythagorean minor seventh (16/9)")
    , (pythagoreanMajor7,    "Pythagorean major seventh (243/128)")
    , (pythagoreanLimma,     "Pythagorean limma (256/243)")
    , (pythagoreanApotome,   "Pythagorean apotome (2187/2048)")
    , (pythagoreanTritone,   "Pythagorean tritone (729/512)")
    , (pythagoreanDim5,      "Pythagorean diminished fifth (1024/729)")
    ]
  , -- 5-limit (classic / just)
    [ (justMaj3,             "just major third (5/4)")
    , (justMin3,             "just minor third (6/5)")
    , (justMin6,             "just minor sixth (8/5)")
    , (justMaj6,             "just major sixth (5/3)")
    , (justMaj7,             "just major seventh (15/8)")
    , (largeMin7,            "large minor seventh (9/5)")
    , (justMinorWholeTone,   "classic minor whole tone (10/9)")
    , (justSemitone,         "classic diatonic semitone (16/15)")
    , (justChroma,           "classic chromatic semitone (25/24)")
    , (justDim4,             "classic diminished fourth (32/25)")
    , (justAug5,             "classic augmented fifth (25/16)")
    , (justDim5,             "classic diminished fifth (36/25)")
    , (justTritone,          "diatonic tritone (45/32)")
    , (graveFifth,           "grave fifth (40/27)")
    ]
  , -- 7-limit (septimal)
    [ (subMin3,                   "subminor third (7/6)")
    , (subMin6,                   "subminor sixth (14/9)")
    , (subMin7,                   "subminor seventh (7/4)")
    , (superMaj2,                 "supermajor second (8/7)")
    , (superMaj3,                 "supermajor third (9/7)")
    , (superMaj6,                 "supermajor sixth (12/7)")
    , (aug4,                      "augmented fourth (7/5)")
    , (dim4,                      "diminished fifth (10/7)")
    , (septimalQuarterTone,       "septimal quarter tone (36/35)")
    , (septimalThirdTone,         "septimal third tone (28/27)")
    , (septimalChromaticSemitone, "septimal chromatic semitone (21/20)")
    , (septimalDiatonicSemitone,  "septimal diatonic semitone (15/14)")
    , (septimalNeutral2,          "septimal neutral second (35/32)")
    , (smallerSeptimalNeutral3,   "smaller septimal neutral third (60/49)")
    , (sub4,                      "sub-fourth (21/16)")
    , (septimalSuper4,            "septimal super-fourth (48/35)")
    , (super5,                    "super-fifth (32/21)")
    , (septimalSuboctave,         "septimal suboctave (63/32)")
    ]
  , -- 11-limit (undecimal)
    [ (undecimalNeutral3,   "undecimal neutral third (11/9)")
    , (undecimalNeutral6,   "undecimal neutral sixth (18/11)")
    , (undecimalSubmin6,    "undecimal subminor sixth (11/7)")
    , (super4,              "super-fourth (11/8)")
    , (undecimalQuarterTone,"undecimal quarter tone (33/32)")
    , (undecimalNeutral2,   "undecimal neutral second (11/10)")
    , (undecimalMajor3,     "undecimal major third (14/11)")
    , (undecimalAug4,       "undecimal augmented fourth (15/11)")
    , (undecimalDim5,       "undecimal diminished fifth (22/15)")
    , (undecimalSub5,       "undecimal sub-fifth (16/11)")
    , (undecimalNeutral7,   "undecimal neutral seventh (11/6)")
    , (undecimalMajor7,     "undecimal major seventh (21/11)")
    ]
  , -- 13-limit (tridecimal)
    [ (tridecimalDim5,             "tridecimal diminished fifth (13/9)")
    , (lesserTridecimalNeutral6,   "lesser tridecimal neutral sixth (13/8)")
    , (tridecimalSubmaj7,          "tridecimal submajor seventh (13/7)")
    , (tridecimalNeutral2,         "tridecimal neutral second (13/12)")
    , (tridecimalMinor3,           "tridecimal minor third (13/11)")
    , (tridecimalSubmaj3,          "tridecimal submajor third (26/21)")
    , (greaterTridecimalNeutral3,  "greater tridecimal neutral third (16/13)")
    , (tridecimalAug4,             "tridecimal augmented fourth (18/13)")
    , (tridecimalSuper5,           "tridecimal super-fifth (26/17)")
    , (tridecimalSupraminor6,      "tridecimal supraminor sixth (21/13)")
    , (tridecimalNeutral7,         "tridecimal neutral seventh (24/13)")
    ]
  , -- 17-limit (septendecimal)
    [ (septendecimalSemitone,  "septendecimal semitone (17/16)")
    , (septendecimalWholeTone, "septendecimal whole tone (17/15)")
    , (septendecimalSub4,      "septendecimal sub-fourth (17/13)")
    , (septendecimalTritone,   "septendecimal tritone (17/12)")
    , (septendecimalSubmin6,   "septendecimal subminor sixth (17/11)")
    , (septendecimalDim7,      "septendecimal diminished seventh (17/10)")
    , (septendecimalMin7,      "septendecimal minor seventh (30/17)")
    ]
  , -- 19-limit (undevicesimal)
    [ (undevicesimalSemitone, "undevicesimal semitone (20/19)")
    , (undevicesimalMinor3,   "undevicesimal minor third (19/16)")
    , (undevicesimalMajor3,   "undevicesimal major third (24/19)")
    , (undevicesimalTritone,  "undevicesimal tritone (27/19)")
    , (undevicesimalMajor6,   "undevicesimal major sixth (32/19)")
    ]
  , -- 23-limit (vicesimotertial)
    [ (vicesimotertialSemitone, "vicesimotertial semitone (24/23)")
    , (vicesimotertialNeutral3, "vicesimotertial neutral third (28/23)")
    , (vicesimotertialSuper4,   "vicesimotertial superaugmented fourth (23/16)")
    ]
  , -- >= 29-limit (octave-reduced harmonics)
    [ (harmonic29, "octave-reduced 29th harmonic (29/16)")
    , (harmonic31, "octave-reduced 31st harmonic (31/16)")
    , (harmonic41, "octave-reduced 41st harmonic (41/32)")
    , (harmonic61, "octave-reduced 61st harmonic (61/32)")
    , (harmonic97, "octave-reduced 97th harmonic (97/64)")
    ]
  , -- Just intonation chords (unfolded)
    justChordNames
  , -- Octave-doubling variants of chords; they share the parent's name.
    doublingPairs
  ]

-- | Exact-value name lookup table (space for time for 'polyName').
exactNameTable :: M.Map Poly T.Text
exactNameTable = M.fromList polyNames

-- | Human-readable name of a corpus poly. Exact lookup first, then
--   near-exact (within 1.2e-15 to absorb float drift), then approximate
--   match within 'fepsilog' (1 cent) via 'polyApprox'.
polyName :: Poly -> Maybe T.Text
polyName p =
  case M.lookup p exactNameTable of
    Just n  -> Just n
    Nothing -> case L.find (\ (k, _) -> polyUlp p k) polyNames of
      Just (_, n) -> Just n
      Nothing     -> case L.find (\ (k, _) -> polyApprox p k) polyNames of
        Just (_, n) -> Just n
        Nothing     -> Nothing

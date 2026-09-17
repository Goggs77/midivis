{-# LANGUAGE BangPatterns #-}
-- | Declarative modulation matrix: how the mod sources (CC, ENV1..3, LFOs,
--   note length) drive the targets (voice amplitude / frequency / vibrato
--   depth, and every live-modulatable effect parameter).  The matrix is pure
--   data stored in the preset ('vMatrix'); the render callback evaluates it
--   once per block — zero allocation (linear scans over a small list).
module Midivis.Synth.ModMatrix
    ( ModSource(..)
    , ModTarget(..)
    , ModRow(..)
    , EqParam(..)
    , FilterShape(..)
    , FilterParam(..)
    , shapeOfFloat
    , isPerVoice
    ) where

-- | Modulation sources.
data ModSource
    = SrcCC   !Int    -- ^ CC 0..127 (normalised [0,1])
    | SrcEnv1         -- ^ ENV1 (volume envelope) output [0,1]
    | SrcEnv2         -- ^ ENV2 output [0,1]
    | SrcEnv3         -- ^ ENV3 output [0,1]
    | SrcLfo  !Int    -- ^ LFO index (in [-1,1])
    | SrcNoteLen      -- ^ note-length map output [0,1]
    deriving (Eq, Show)

-- | Which parameter of a 12-band EQ band is modulated.
data EqParam
    = EqFreq          -- ^ band centre frequency (Hz)
    | EqGain          -- ^ band gain (dB)
    | EqShape         -- ^ band shape (0..1 -> 7 filter types)
    | EqQ             -- ^ band Q
    deriving (Eq, Show)

-- | Filter shapes, one per 1/7 slice of the 0..1 shape parameter
--   (order matches the C++ EQ core's @enum Shape@).
data FilterShape
    = FLowShelf
    | FPeak
    | FHighShelf
    | FNotch
    | FBandpass
    | FLowpass
    | FHighpass
    deriving (Eq, Show)

-- | Map a 0..1 shape parameter to a 'FilterShape' (7 piecewise types,
--   exactly like the C++ core's shapeOf: @int(7 * clamp01 s)@, capped at 6).
shapeOfFloat :: Double -> FilterShape
shapeOfFloat s =
    case min 6 (floor (7.0 * clamp01 s)) of
        0 -> FLowShelf
        1 -> FPeak
        2 -> FHighShelf
        3 -> FNotch
        4 -> FBandpass
        5 -> FLowpass
        _ -> FHighpass
  where
    clamp01 x = min 1 (max 0 x)

-- | Which parameter of a per-voice filter stage is modulated.
data FilterParam
    = FCutoff         -- ^ filter cutoff frequency (Hz)
    | FGain           -- ^ filter gain (dB, shelf/peak)
    | FShape          -- ^ filter shape (0..1 -> 'FilterShape')
    | FQ              -- ^ filter resonance / Q
    deriving (Eq, Show)

-- | Modulation targets.  Per-voice targets are evaluated per sounding slot;
--   effect-parameter targets are evaluated once per block with the aggregate
--   source values (max over the sounding voices).
data ModTarget
    = TgtAmp           -- ^ voice amplitude gain (× (1 + amount))
    | TgtFreq          -- ^ voice frequency ratio (× (1 + amount))
    | TgtVibDepth      -- ^ vibrato depth (additive)
    | TgtEqBand !Int !EqParam  -- ^ 12-band EQ parameter, band 0..11 (additive)
    | TgtVFilter !Int !FilterParam  -- ^ per-voice filter stage 0|1 parameter (additive)
    | TgtChorusMix     -- ^ chorus wet mix
    | TgtEnsembleWidth -- ^ ensemble width
    | TgtAlgWet        -- ^ algorithmic reverb wet
    | TgtAlgRoom       -- ^ algorithmic reverb room
    | TgtConvWet       -- ^ convolution reverb wet
    | TgtConvDry       -- ^ convolution reverb dry
    | TgtCompThr       -- ^ compressor threshold (dBFS)
    | TgtWiden         -- ^ stereo widener a
    deriving (Eq, Show)

-- | One modulation row: source scaled by @mrAmount@ into the target's base
--   value.  A negative amount inverts the source.
data ModRow = ModRow
    { mrSource :: !ModSource
    , mrTarget :: !ModTarget
    , mrAmount :: !Double
    } deriving (Eq, Show)

-- | Whether the row targets a per-voice value (as opposed to a shared
--   effect parameter).
isPerVoice :: ModRow -> Bool
isPerVoice r = case mrTarget r of
    TgtAmp      -> True
    TgtFreq     -> True
    TgtVibDepth -> True
    TgtVFilter _ _ -> True
    _           -> False

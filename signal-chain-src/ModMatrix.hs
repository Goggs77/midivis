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

-- | Modulation targets.  Per-voice targets are evaluated per sounding slot;
--   effect-parameter targets are evaluated once per block with the aggregate
--   source values (max over the sounding voices).
data ModTarget
    = TgtAmp           -- ^ voice amplitude gain (× (1 + amount))
    | TgtFreq          -- ^ voice frequency ratio (× (1 + amount))
    | TgtVibDepth      -- ^ vibrato depth (additive)
    | TgtEqLow         -- ^ EQ low-shelf gain, dB (additive)
    | TgtEqMid         -- ^ EQ peak gain, dB
    | TgtEqHigh        -- ^ EQ high-shelf gain, dB
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
    _           -> False

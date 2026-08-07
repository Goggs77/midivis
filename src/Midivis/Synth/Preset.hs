{-# LANGUAGE BangPatterns #-}
-- | Synthesizer voices and the per-voice sample functions.
--
--   A 'Voice' is a self-contained sound: the waveform is a direct function
--   @SynthCtx -> Double@ where the voice's own parameters are written
--   straight into the formula (no shared parameter template — every voice
--   may use a completely different harmonic structure).  Alongside the
--   sample function a voice carries its envelope (drives the ADSR state
--   machine in Render / Engine) and its LFO set (block-constant modulators).
--
--   Modulation context, following the chain
--     freq → phase → ccbus → lfo → amp/noise → sample:
--       scFreq  voice frequency (Hz)
--       scPhase phase in cycles (0..1), already advanced by freq
--       scCC    CC bus snapshot, normalised [0,1], index = CC no.
--       scLfos  LFO values, one per vLfos entry, in [-1,1]
--       scAmp   smoothed amplitude (envelope output, 0..1)
--       scNoise deterministic noise in [-depth, depth]
--
--   The callback stays allocation-free: LFO values are block-constant, the
--   CC snapshot is read once per block, and the sample function is a pure
--   fold.
module Midivis.Synth.Preset
    ( Voice(..), ReverbParams(..), LfoParams(..), EnvParams(..)
    , SynthCtx(..)
    , flute, piano
    , smoothNoise
    ) where

import Prelude
import Data.Bits (shiftR, (.&.))
import Midivis.Util.Math (log10)
import Midivis.Synth.AlgorithmicReverb (AlgParams(..))
import Midivis.Synth.Types
    ( defNormalizeAmp
    , defLFORate1, defLFORate2, defLFORate3
    , defLFODepth1, defLFODepth2, defLFODepth3
    , defLFOSeed1, defLFOSeed2, defLFOSeed3
    , defAmpSmoothK, defAttack, defDecay, defSustain, defRelease
    , EnvelopeState(..)
    )

--------------------------------------------------------------------------------
-- Voice structure
--------------------------------------------------------------------------------

-- | One LFO: sample-and-hold rate, depth (modulation amount), seed, and a
--   base around which it modulates (m = base + lfo·depth; the flute's 3rd
--   LFO uses base 0.79 so its partial never fully disappears).
data LfoParams = LfoParams
    { lpRate  :: !Double
    , lpDepth :: !Double
    , lpSeed  :: !Int
    , lpBase  :: !Double
    }

-- | Envelope segment lengths, curve exponents and the aftertouch smoothing
--   coefficient.  Segment fields are Float to match 'EnvelopeState' exactly
--   (they build the initial states); the smoothing coefficient stays Double.
data EnvParams = EnvParams
    { epAttack       :: !Float
    , epAttackCurve  :: !Float
    , epDecay        :: !Float
    , epDecayCurve   :: !Float
    , epSustainLevel :: !Float
    , epRelease      :: !Float
    , epReleaseCurve :: !Float
    , epAmpSmoothK   :: !Double
    }

-- | A complete voice: its own sample formula, envelope, LFO set, and the
--   reverb mix for its ambience.  'vSample' gets full freedom — parameters
--   are written directly into the formula, so voices can differ
--   structurally, not just numerically.
data Voice = Voice
    { vSample :: !(SynthCtx -> Double)
    , vEnv    :: !EnvParams
    , vLfos   :: ![LfoParams]
    , vReverb :: !ReverbParams
    }

-- | The two reverb inserts, per voice.  The algorithmic reverb (Freeverb
--   pre-space, before the convolution) and the convolution reverb
--   (dry/wet/gain) are both assembled from these numbers, so switching
--   voices changes the ambience along with the timbre.
data ReverbParams = ReverbParams
    { rvAlg      :: !AlgParams   -- algorithmic reverb (Freeverb) insert
    , rvConvDry  :: !Double      -- convolution reverb dry mix
    , rvConvWet  :: !Double      -- convolution reverb wet mix
    , rvConvGain :: !Double      -- convolution reverb master gain
    }

--------------------------------------------------------------------------------
-- Per-sample modulation context
--------------------------------------------------------------------------------

-- | Everything a 'vSample' needs for one sample.  All fields are plain
--   values — the callback assembles this struct once per sample with zero
--   allocation.
data SynthCtx = SynthCtx
    { scFreq  :: !Double    -- voice frequency (Hz)
    , scPhase :: !Double    -- phase in cycles (0..1), already advanced by freq
    , scCC    :: ![Double]  -- CC bus snapshot, normalised [0,1], index = CC no.
    , scLfos  :: ![Double]  -- LFO values, one per vLfos entry, in [-1,1]
    , scAmp   :: !Double    -- smoothed amplitude (envelope output, 0..1)
    , scNoise :: !Double    -- deterministic noise in [-depth, depth]
    }

-- | Safe index into the LFO value list (missing → 0, i.e. no modulation).
lfoAt :: SynthCtx -> Int -> Double
lfoAt ctx i = case drop i (scLfos ctx) of (x:_) -> x; [] -> 0.0

--------------------------------------------------------------------------------
-- Flute voice (the original hand-tuned sound)
--------------------------------------------------------------------------------

-- | Soft sustained voice: slow attack, near-sinusoidal with a strong 3rd
--   partial, gentle LFO "breathing".  Parameters are inlined in the formula.
flute :: Voice
flute = Voice
    { vSample = \ctx ->
        let !f  = scFreq ctx
            !ph = scPhase ctx
            !an = scAmp ctx * defNormalizeAmp
            !cc = scCC ctx
            !mw = ((cc !! 1) * 1.3 + 0.75)
            !noise = scNoise ctx
            -- LFO modulators (depth/base from the flute's character)
            !m1 = 1.0 + lfoAt ctx 0 * 0.18
            !m2 = 1.0 + lfoAt ctx 1 * 0.19
            !m3 = 0.79 + lfoAt ctx 2 * 0.56
            !x  = 
                ( m1 * sin (2 * pi * ph) * 0.8   * (- 0.22 * log10 f + 1.2)
                + m2 * sin (4 * pi * ph) * 0.02 * an * (- 0.34 * log10 f + 1.34)
                + m3 * sin (6 * pi * ph) * 0.314 * an * (- 0.17 * log10 f + 1.17)
                + m2 * sin (6 * pi * ph * (noise + 1)) * 0.0114 * an * an * an ) * m1 * 1.2 * mw
        in tanh x * an
    , vEnv = EnvParams
        { epAttack       = atkDur
        , epAttackCurve  = atkCurve
        , epDecay        = decDur
        , epDecayCurve   = decCurve
        , epSustainLevel = susLvl
        , epRelease      = relDur
        , epReleaseCurve = relCurve
        , epAmpSmoothK   = defAmpSmoothK
        }
    , vLfos = [ LfoParams defLFORate1 defLFODepth1 defLFOSeed1 1.0
              , LfoParams defLFORate2 defLFODepth2 defLFOSeed2 1.0
              , LfoParams defLFORate3 defLFODepth3 defLFOSeed3 0.79
              ]
    -- Warm medium room: modest predelay, gentle damping so the sustained
    -- tone keeps its body without mudding up.
    , vReverb = ReverbParams
        { rvAlg = AlgParams
            { apRoom     = 0.6
            , apDamp     = 0.35
            , apSpread   = 0.5
            , apPreDelay = 0.012
            , apWet      = 0.2
            , apDry      = 0.95
            }
        , rvConvDry  = 0.85
        , rvConvWet  = 0.3
        , rvConvGain = 1.2
        }
    }
  where
    Attack atkDur atkCurve = defAttack
    Decay  decDur decCurve = defDecay
    Sustain susLvl = defSustain
    Release relDur relCurve = defRelease

--------------------------------------------------------------------------------
-- Piano voice
--------------------------------------------------------------------------------

-- | Struck voice: near-instant attack, fast decay to a low sustain, long
--   release tail; richer upper harmonics than the flute, barely any LFO
--   wobble (a struck string has no vibrato), a touch of hammer noise.
piano :: Voice
piano = Voice
    { vSample = \ctx ->
        let !f  = scFreq ctx
            !ph = scPhase ctx
            !an = scAmp ctx * defNormalizeAmp
            !cc = scCC ctx
            !noise = scNoise ctx
            !m1 = 1.0 + lfoAt ctx 0 * 0.02
            !m2 = 1.0 + lfoAt ctx 1 * 0.04
            !m3 = 0.9 + lfoAt ctx 2 * 0.08
            !filt1  = min (- 0.05 * log10 (f/2000) + 1.05) 1
            !x  = 
                ( sin (2 * pi * ph) * 1.000   -- 1st
                + sin (4 * pi * ph + 0.8492) * 0.600   -- 2nd
                + sin (6 * pi * ph + 0.2835) * 0.400   -- 3rd
                + sin (8 * pi * ph + 0.9574) * 0.280   -- 4th
                + sin (10 * pi * ph+ 0.3234) * 0.200  -- 5th
                + sin (12 * pi * ph) * 0.150  -- 6th
                + sin (14 * pi * ph) * 0.110  -- 7th
                + sin (16 * pi * ph) * 0.080  -- 8th
                -- detail harmonics
                + sin (18 * pi * ph) * 0.060  * (-an + 1.20 * (cc !! 1))-- 9th
                + sin (20 * pi * ph) * 0.045  * (-an + 1.16 * (cc !! 1))-- 10th
                + sin (22 * pi * ph) * 0.035  * (-an + 1.13 * (cc !! 1))-- 11th
                + sin (24 * pi * ph) * 0.025  * (-an + 1.11 * (cc !! 1))-- 12th
                + sin (26 * pi * ph) * 0.018  * (-an + 1.10 * (cc !! 1))-- 13th
                + sin (28 * pi * ph) * 0.013  * (-an + 1.10 * (cc !! 1)) -- 14th
                + sin (30 * pi * ph) * 0.010  * (-an + 1.10 * (cc !! 1)) -- 15th
                + sin (32 * pi * ph) * 0.007  * (-an + 1.10 * (cc !! 1)) -- 16th
                + sin (34 * pi * ph) * 0.005  * (-an + 1.10 * (cc !! 1)) -- 17th
                + sin (36 * pi * ph) * 0.004  * (-an + 1.10 * (cc !! 1)) -- 18th
                + sin (38 * pi * ph) * 0.003  * (-an + 1.10 * (cc !! 1)) -- 19th
                + sin (40 * pi * ph) * 0.002  * (-an + 1.10 * (cc !! 1)) -- 20th
                + sin (42 * pi * ph) * 0.0015 * (-an + 1.10 * (cc !! 1)) -- 21st
                + sin (44 * pi * ph) * 0.0010 * (-an + 1.10 * (cc !! 1)) -- 22nd
                + sin (46 * pi * ph) * 0.0007 * (-an + 1.10 * (cc !! 1)) -- 23rd
                + sin (48 * pi * ph) * 0.0005 * (-an + 1.09 * (cc !! 1)) -- 24th
                ) / 2.0-- / 3.271
        in tanh x * an
    , vEnv = EnvParams
        { epAttack       = 0.02
        , epAttackCurve  = 0.05     -- was 1.0: deltaPower power=10^1 → attack iteration exploded, amp overshot then went NaN
        , epDecay        = 0.11
        , epDecayCurve   = 0.4     -- was 1.0: power=10^1 → first decay block collapsed amp to 0 (silence)
        , epSustainLevel = 0.0
        , epRelease      = 0.1
        , epReleaseCurve = 0.1
        , epAmpSmoothK   = 1 -- no smooth at all
        }
    , vLfos = [ LfoParams 0.8 0.02 defLFOSeed1 1.0
              , LfoParams 1.7 0.04 defLFOSeed2 1.0
              , LfoParams 2.5 0.08 defLFOSeed3 0.9
              ]
    -- Bright hall: longer predelay (the hammer transient needs room before
    -- the tail), less damping, more wet — the long release tail rides the
    -- reverb naturally.
    , vReverb = ReverbParams
        { rvAlg = AlgParams
            { apRoom     = 0.77
            , apDamp     = 0.96
            , apSpread   = 0.4
            , apPreDelay = 0.02
            , apWet      = 0.3
            , apDry      = 1
            }
        , rvConvDry  = 1
        , rvConvWet  = 0.1
        , rvConvGain = 1.2
        }
    }

--------------------------------------------------------------------------------
-- LFO generation
--------------------------------------------------------------------------------

-- | Deterministic smooth-random LFO value in [-1, 1] at absolute sample
--   position @sampleCount@.  Sample-and-hold segments at @rate@ Hz joined by
--   linear interpolation; the hash is a wrapping Int multiply so no heap
--   allocation happens in the audio callback.
smoothNoise :: Double -> Double -> Double -> Int -> Double
smoothNoise sampleCount sr rate seed =
    let pos  = sampleCount / sr * rate
        seg  = floor pos :: Int
        frac = pos - fromIntegral seg
        v0   = noiseAt seg
        v1   = noiseAt (seg + 1)
    in v0 + (v1 - v0) * frac
  where
    noiseAt :: Int -> Double
    noiseAt s =
        let h = (fromIntegral s * 2654435761 + fromIntegral seed * 40503) * 1103515245 :: Int
            u = fromIntegral ((h `shiftR` 16) .&. 0xFFFFFF) :: Double
        in u / 8388607.5 - 1.0

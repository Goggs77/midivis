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
    ( Voice(..), ReverbParams(..), LfoParams(..), EnvParams(..), FxParams(..), NoteLenMap(..), ModRow(..)
    , defFxParams, defNoteLenMap, noteLenAt
    , SynthCtx(..)
    , flute, piano, allPresets
    , smoothNoise
    ) where

import Prelude
import Data.Bits (shiftR, (.&.))
import Midivis.Util.Math (log10)
import Midivis.Synth.AlgorithmicReverb (AlgParams(..))
import Midivis.Synth.ModMatrix (ModRow(..), ModSource(..), ModTarget(..))
import Midivis.Synth.Types
    ( defNormalizeAmp
    , defLFORate1, defLFORate2, defLFORate3
    , defLFODepth1, defLFODepth2, defLFODepth3
    , defLFOSeed1, defLFOSeed2, defLFOSeed3
    , defAmpSmoothK, defAttack, defDecay, defSustain, defRelease
    , defStereoWidth
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

-- | Note-length modulation map: how long a note has been held (seconds) maps
--   to a [0,1] modulation amount, piecewise-linear ("non-periodic linear",
--   the mapping function is defined per preset).  @nlmBreakpoints@ is a
--   sorted list of (seconds, value) knots; beyond @nlmTmax@ the last value
--   saturates.  The map is pure data — evaluating it is a linear scan, zero
--   allocation, safe for the real-time path.
data NoteLenMap = NoteLenMap
    { nlmBreakpoints :: [(Double, Double)]
    , nlmTmax        :: !Double
    }

-- | Default: flat zero — no note-length modulation until a preset defines
--   breakpoints.
defNoteLenMap :: NoteLenMap
defNoteLenMap = NoteLenMap [(0, 0)] 0

-- | Evaluate the map at @t@ seconds: linear interpolation between
--   breakpoints, saturation beyond Tmax, and 0 for an empty/zero map.
noteLenAt :: NoteLenMap -> Double -> Double
noteLenAt (NoteLenMap [] _) _ = 0
noteLenAt (NoteLenMap bps tmax) t
    | t <= t0     = v0
    | t >= tmax   = lastV
    | otherwise   = go bps
  where
    (t0, v0) = head bps
    lastV    = snd (last bps)
    go ((ta, va) : (tb, vb) : rest)
        | t < tb    = va + (vb - va) * (t - ta) / (tb - ta)
        | otherwise = go ((tb, vb) : rest)
    go _ = lastV

-- | A complete voice: its own sample formula, three envelopes, LFO set, and
--   the reverb mix for its ambience.  'vSample' gets full freedom — parameters
--   are written directly into the formula, so voices can differ
--   structurally, not just numerically.
--   @vEnv@ is the volume (main) envelope, ENV1; @vEnv2@/@vEnv3@ are
--   independent modulation envelopes whose outputs (0..1) drive modulation
--   targets via the preset's modulation matrix.
data Voice = Voice
    { vSample :: !(SynthCtx -> Double)
    , vEnv    :: !EnvParams
    , vEnv2   :: !EnvParams
    , vEnv3   :: !EnvParams
    , vLfos   :: ![LfoParams]
    , vFx     :: !FxParams
    , vNoteLen :: !NoteLenMap
    , vMatrix  :: ![ModRow]
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

-- | Effect-chain base parameters for one voice (the modulation matrix adds
--   CC/ENV/LFO/notelen amounts on top of these at render time, so every
--   field doubles as a modulation target).
data FxParams = FxParams
    { fxEqLowGain     :: !Double   -- dB, low shelf
    , fxEqMidGain     :: !Double   -- dB, peak
    , fxEqHighGain    :: !Double   -- dB, high shelf
    , fxEqMidFreq     :: !Double   -- Hz, peak centre
    , fxEqMidQ        :: !Double   -- peak Q
    , fxChorusMix     :: !Double   -- [0,1] wet mix
    , fxChorusDepth   :: !Double   -- delay modulation depth (samples)
    , fxChorusRate    :: !Double   -- Hz
    , fxEnsembleWidth :: !Double   -- L/R base delay offset (samples)
    , fxVibDepth      :: !Double   -- pitch vibrato depth (ratio, 0 = off)
    , fxVibRate       :: !Double   -- Hz
    , fxCompThr       :: !Double   -- dBFS
    , fxCompRatio     :: !Double   -- compression ratio
    , fxCompAtk       :: !Double   -- seconds
    , fxCompRel       :: !Double   -- seconds
    , fxWiden         :: !Double   -- widener allpass a
    , fxDcR           :: !Double   -- DC blocker coefficient
    }

-- | Neutral FX defaults: every effect off / passthrough, compressor at the
--   current hardcoded settings — behaviour is unchanged until a preset
--   sets something else.
defFxParams :: FxParams
defFxParams = FxParams
    { fxEqLowGain     = 0.0
    , fxEqMidGain     = 0.0
    , fxEqHighGain    = 0.0
    , fxEqMidFreq     = 1000.0
    , fxEqMidQ        = 1.0
    , fxChorusMix     = 0.0
    , fxChorusDepth   = 0.0
    , fxChorusRate    = 0.8
    , fxEnsembleWidth = 0.0
    , fxVibDepth      = 0.0
    , fxVibRate       = 5.0
    , fxCompThr       = -6.0
    , fxCompRatio     = 100.0
    , fxCompAtk       = 0.001
    , fxCompRel       = 0.325
    , fxWiden         = defStereoWidth
    , fxDcR           = 0.9995
    }

--------------------------------------------------------------------------------
-- Per-sample modulation context
--------------------------------------------------------------------------------

-- | Everything a 'vSample' needs for one sample.  All fields are plain
--   values — the callback assembles this struct once per sample with zero
--   allocation.
data SynthCtx = SynthCtx
    { scFreq    :: !Double    -- voice frequency (Hz)
    , scPhase   :: !Double    -- phase in cycles (0..1), already advanced by freq
    , scCC      :: ![Double]  -- CC bus snapshot, normalised [0,1], index = CC no.
    , scLfos    :: ![Double]  -- LFO values, one per vLfos entry, in [-1,1]
    , scAmp     :: !Double    -- smoothed amplitude (envelope output, 0..1)
    , scNoise   :: !Double    -- deterministic noise in [-depth, depth]
    , scNoteLen :: !Double    -- note-length modulation amount [0,1] (NoteLenMap)
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
            !vel = ((cc !! 128) + 0.01)
            !noise = scNoise ctx
            -- LFO modulators (depth/base from the flute's character)
            !m1 = 1.0 + lfoAt ctx 0 * 0.18 * mw
            !m2 = 1.0 + lfoAt ctx 1 * 0.19
            !m3 = 0.79 + lfoAt ctx 2 * 0.56
            !x  = 
                ( m1 * sin (2 * pi * ph) * 0.8   * (- 0.22 * log10 f + 1.2)
                + m2 * sin (4 * pi * ph) * 0.02 * an * (- 0.34 * log10 f + 1.34)
                + m3 * sin (6 * pi * ph) * 0.314 * an * (- 0.17 * log10 f + 1.17)
                + m2 * sin (6 * pi * ph * (noise + 1)) * 0.0114 * an * an * an ) * m1 * 1.7 * mw * vel
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
    , vEnv2 = EnvParams
        { epAttack = 0.5, epAttackCurve = 0.5, epDecay = 0.5, epDecayCurve = 0.5
        , epSustainLevel = 0.5, epRelease = 0.5, epReleaseCurve = 0.5, epAmpSmoothK = defAmpSmoothK }
    , vEnv3 = EnvParams
        { epAttack = 0.5, epAttackCurve = 0.5, epDecay = 0.5, epDecayCurve = 0.5
        , epSustainLevel = 0.5, epRelease = 0.5, epReleaseCurve = 0.5, epAmpSmoothK = defAmpSmoothK }
    , vFx = defFxParams
    , vNoteLen = defNoteLenMap
    -- Demo modulation: mod wheel drives the chorus mix, note length brightens
    -- the high shelf (longer notes get airier).
    , vMatrix = [ ModRow (SrcCC 1) TgtChorusMix 0.5
                , ModRow SrcNoteLen TgtEqHigh 3.0
                ]
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
    , vEnv2 = EnvParams
        { epAttack = 0.5, epAttackCurve = 0.5, epDecay = 0.5, epDecayCurve = 0.5
        , epSustainLevel = 0.5, epRelease = 0.5, epReleaseCurve = 0.5, epAmpSmoothK = defAmpSmoothK }
    , vEnv3 = EnvParams
        { epAttack = 0.5, epAttackCurve = 0.5, epDecay = 0.5, epDecayCurve = 0.5
        , epSustainLevel = 0.5, epRelease = 0.5, epReleaseCurve = 0.5, epAmpSmoothK = defAmpSmoothK }
    , vFx = defFxParams
    , vNoteLen = defNoteLenMap
    -- Demo modulation: note length deepens the vibrato, the mod wheel opens
    -- the algorithmic reverb, ENV3 swells the convolution wet.
    , vMatrix = [ ModRow SrcNoteLen TgtVibDepth 0.002
                , ModRow (SrcCC 1) TgtAlgWet 0.4
                , ModRow SrcEnv3 TgtConvWet 0.3
                ]
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


-- | All selectable presets, in switch order (',' / '.' scroll through them).
allPresets :: [Voice]
allPresets = [ flute, piano ]

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
    ( Voice(..), ReverbParams(..), LfoParams(..), EnvParams(..), FxParams(..), EqBand(..), FilterParams(..), NoteLenMap(..), ModRow(..)
    , defFxParams, defNoteLenMap, defEqBands, defFilterParams, noteLenAt
    , SynthCtx(..)
    , flute, piano, allPresets
    , smoothNoise
    ) where

import Prelude
import Data.Bits (shiftR, (.&.))
import Midivis.Util.Math (log10)
import Midivis.Synth.AlgorithmicReverb (AlgParams(..))
import Midivis.Synth.ModMatrix (ModRow(..), ModSource(..), ModTarget(..), EqParam(..), FilterParam(..))
import Midivis.Synth.Sine
import Midivis.Synth.Types
    ( defNormalizeAmp
    , defLFORate1, defLFORate2, defLFORate3
    , defLFODepth1, defLFODepth2, defLFODepth3
    , defLFOSeed1, defLFOSeed2, defLFOSeed3
    , defAmpSmoothK, defAttack, defDecay, defSustain, defRelease
    , defStereoWidth
    , EnvelopeState(..)
    )
import Midivis.Synth.KeyboardLayout (KeyboardLayout(MiniLab3), CtrlRole (..), roleToCC)

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
    , vFilter :: !(FilterParams, FilterParams)  -- per-voice filter stages 0|1
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

-- | One 12-band-EQ band: centre frequency, gain (dB), shape (0..1, mapped
--   piecewise to 7 filter types by the DSP core), Q.
--   C++: enum Shape { kLowShelf, kPeak, kHighShelf, kNotch, kBandpass, kLowpass, kHighpass }
data EqBand = EqBand
    { ebFreq  :: !Double
    , ebGain  :: !Double
    , ebShape :: !Double
    , ebQ     :: !Double
    } deriving (Eq, Show)

-- | Default neutral layout: 10 peaking bands log-spaced 32 Hz..16 kHz
--   between a low shelf (20 Hz) and a high shelf (20 kHz), all gains 0 dB —
--   every band is smart-bypassed, leaving only the Nyquist guard.
defEqBands :: [EqBand]
defEqBands =
    [ EqBand 20.0    0.0 0.0        0.707  -- 0  low shelf
    , EqBand 32.0    0.0 (1/7)      1.0    -- 1  peak
    , EqBand 64.0    0.0 (1/7)      1.0    -- 2
    , EqBand 128.0   0.0 (1/7)      1.0    -- 3
    , EqBand 256.0   0.0 (1/7)      1.0    -- 4
    , EqBand 512.0   0.0 (1/7)      1.0    -- 5
    , EqBand 1024.0  0.0 (1/7)      1.0    -- 6
    , EqBand 2048.0  0.0 (1/7)      1.0    -- 7
    , EqBand 4096.0  0.0 (1/7)      1.0    -- 8
    , EqBand 8192.0  0.0 (1/7)      1.0    -- 9
    , EqBand 16384.0 0.0 (1/7)      1.0    -- 10
    , EqBand 20000.0 0.0 (2/7)      0.707  -- 11 high shelf
    ]

-- | One per-voice filter stage: cutoff (Hz), gain (dB), shape (0..1, mapped
--   to a 'Midivis.Synth.ModMatrix.FilterShape' by 'shapeOfFloat'), Q.  Two
--   stages per voice ('vFilter'), applied to each voice's output before it
--   joins the mix; the modulation matrix modulates every field per-voice
--   via 'TgtVFilter'.
data FilterParams = FilterParams
    { fpCutoff :: !Double
    , fpGain   :: !Double
    , fpShape  :: !Double
    , fpQ      :: !Double
    } deriving (Eq, Show)

-- | Neutral: peaking @ 20 kHz, 0 dB.  A gain-0 RBJ peak is a straight wire,
--   so both stages are transparent until a preset (or the matrix) moves them.
defFilterParams :: FilterParams
defFilterParams = FilterParams
    { fpCutoff = 20000.0
    , fpGain   = 0.0
    , fpShape  = 1.0 / 7.0   -- peak
    , fpQ      = 0.707
    }

-- | Effect-chain base parameters for one voice (the modulation matrix adds
--   CC/ENV/LFO/notelen amounts on top of these at render time, so every
--   field doubles as a modulation target).
data FxParams = FxParams
    { fxEqBands      :: ![EqBand]   -- ^ 12-band EQ (freq/gain/shape/Q per band)
    , fxChorusMix    :: !Double   -- [0,1] wet mix
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
    { fxEqBands      = defEqBands
    , fxChorusMix    = 0.0
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
    , scAmp     :: !Double    -- smoothed amplitude (ENV1 output, 0..1)
    , scEnv2    :: !Double    -- ENV2 modulation envelope output [0,1] (block-constant)
    , scEnv3    :: !Double    -- ENV3 modulation envelope output [0,1] (block-constant)
    , scNoise   :: !Double    -- deterministic noise in [-depth, depth]
    , scNoise'  :: !Double    -- deterministic WHITE noise (flat power spectrum)
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
                ( m1 * sine (1*f) (ph) * 0.8   * (- 0.22 * log10 f + 1.2)
                + m2 * sine (2*f) (2*ph) * 0.02 * an * (- 0.34 * log10 f + 1.34)
                + m3 * sine (3*f) (3*ph) * 0.314 * an * (- 0.17 * log10 f + 1.17)
                + m2 * sine (3*f*(noise+1)) (3*ph*(noise+1)) * 0.0114 * an * an * an ) * m1 * mw * vel
        in tanh x * an
    , vFilter = (defFilterParams, defFilterParams)
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
        {
            fxChorusMix = 0.2,
            fxEnsembleWidth = 0.21
            --fxVibDepth = 0.2
        }
    , vNoteLen = defNoteLenMap
    -- Demo modulation: mod wheel drives the chorus mix, note length brightens
    -- the high shelf (longer notes get airier).
    , vMatrix = [ ModRow (SrcCC 1) TgtChorusMix 0.5
                -- note length brightens the high shelf (band 11)
                , ModRow SrcNoteLen (TgtEqBand 11 EqGain) 3.0
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

piano :: Voice
piano = Voice
    { vSample = \ctx ->
        let !f  = scFreq ctx
            !ph = scPhase ctx
            !an = scAmp ctx * defNormalizeAmp
            !env2 = scEnv2 ctx
            !env3 = scEnv3 ctx
            !cc = scCC ctx
            !noise = scNoise' ctx
            !m1 = 1.0 + lfoAt ctx 0 * 0.02
            !x = sawPush (exp(cc!!1 / 3.0)) f (ph) + 11.2*noise +
                -- supersaw generation, most of the magics are consecutive prime ratios
                -- that are indistinguishable from randomRIO under such precision.
                -- reduce DC by multiplying it with another an
                ( saw (f) (ph + 0.9961872484643084)
                + saw (f) (ph + 0.9995765403345331)
                + saw (f) (ph + 0.9987312328187777)
                + saw (f) (ph + 0.9991548700612719)
                + saw (f) (ph + 0.9962113239318038)
                + saw (f) (ph + 0.9983189745744905)
                + saw (f) (ph + 0.9949822287267406)
                + saw (f) (ph + 0.9991644035930646)
                + saw (f) (ph + 0.9995823762789726)
                + saw (f) (ph + 0.9991654496140204)
                + saw (f) (ph + 0.9987497395290685)
                + saw (f) (ph + 0.9995834201208081)
                - saw (f) (ph - 1.0038273442483520)
                - saw (f) (ph - 1.0004236390595214)
                - saw (f) (ph - 1.0012703789964006)
                - saw (f) (ph - 1.0008458447874815)
                - saw (f) (ph - 1.0038030847242763)
                - saw (f) (ph - 1.0016838560303094)
                - saw (f) (ph - 1.0050430762765288)
                - saw (f) (ph - 1.0008362952122098)
                - saw (f) (ph - 1.0004177982034677)
                - saw (f) (ph - 1.0008352474420548)
                - saw (f) (ph - 1.0012518255789693)
                - saw (f) (ph - 1.0004167534903106)
                ) * an
        in max (-1.0) (min 1 (x * an)) * 3.7
    , vFilter = ( 
        --enum Shape { kLowShelf, kPeak, kHighShelf, kNotch, kBandpass, kLowpass, kHighpass };
        FilterParams
        { fpCutoff = 20
        , fpGain   = 0.0
        , fpShape  = 6/7 --high pass
        , fpQ      = 0.707
        }
        , FilterParams
        { fpCutoff = 15000.0
        , fpGain   = 0.0
        , fpShape  = 5/7 --low pass
        , fpQ      = 1
        }
        )
    , vEnv = EnvParams
        { epAttack       = 0.002
        , epAttackCurve  = 0.05     -- was 1.0: deltaPower power=10^1 → attack iteration exploded, amp overshot then went NaN
        , epDecay        = 0.001
        , epDecayCurve   = 0.4     -- was 1.0: power=10^1 → first decay block collapsed amp to 0 (silence)
        , epSustainLevel = 1
        , epRelease      = 0.001
        , epReleaseCurve = 0.5
        , epAmpSmoothK   = 0.7 -- 1 = no smooth at all
        }
    , vEnv2 = EnvParams
        { epAttack = 0.52, epAttackCurve = 0.5, epDecay = 0.001, epDecayCurve = 0.5
        , epSustainLevel = 1.00, epRelease = 1, epReleaseCurve = 0.5, epAmpSmoothK = 0.5 }
    , vEnv3 = EnvParams
        { epAttack = 0.5, epAttackCurve = 0.5, epDecay = 0.5, epDecayCurve = 0.5
        , epSustainLevel = 0.5, epRelease = 0.5, epReleaseCurve = 0.5, epAmpSmoothK = defAmpSmoothK }
    , vFx = FxParams
        { fxEqBands      = 
            [ EqBand 20.0    0.0 (6/7)      0.707  -- 0  low cut
            , EqBand 32.0    0.0 (1/7)      1.0    -- 1  peak
            , EqBand 64.0    0.0 (1/7)      1.0    -- 2
            , EqBand 128.0   0.0 (1/7)      1.0    -- 3
            , EqBand 256.0   0.0 (1/7)      1.0    -- 4
            , EqBand 512.0   0.0 (1/7)      1.0    -- 5
            , EqBand 1024.0  0.0 (1/7)      1.0    -- 6
            , EqBand 2048.0  0.0 (1/7)      1.0    -- 7
            , EqBand 4096.0  0.0 (1/7)      1.0    -- 8
            , EqBand 8192.0  0.0 (1/7)      1.0    -- 9
            , EqBand 16384.0 (-2.0) (2/7)   0.707    -- 10
            , EqBand 18000.0 0.0 (5/7)      0.5    -- 11 soft 18 kHz low-pass
            ]
        , fxChorusMix    = 0.1
        , fxChorusDepth   = 0.8
        , fxChorusRate    = 0.8
        , fxEnsembleWidth = 0.12
        , fxVibDepth      = 0.0
        , fxVibRate       = 5.0
        , fxCompThr       = -5.0
        , fxCompRatio     = 1000.0
        , fxCompAtk       = 0.001
        , fxCompRel       = 1
        , fxWiden         = 0
        , fxDcR           = 0.9995
        }
    , vNoteLen = defNoteLenMap
    -- Demo modulation: note length deepens the vibrato, the mod wheel opens
    -- the algorithmic reverb, ENV3 swells the convolution wet.
    , vMatrix = [ ModRow SrcNoteLen TgtVibDepth 0.002
                --, ModRow SrcEnv2 (TgtVFilter 1 FCutoff) (-12000) 
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
            { apRoom     = 0.8
            , apDamp     = 0.4
            , apSpread   = 0.4
            , apPreDelay = 0.02
            , apWet      = 0.1
            , apDry      = 1
            }
        , rvConvDry  = 1
        , rvConvWet  = 0.1
        , rvConvGain = 0.5
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

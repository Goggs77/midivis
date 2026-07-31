{-# LANGUAGE LambdaCase #-}
module Midivis.Synth.Types where

import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector as SV
import qualified Synthesizer.Storable.Oscillator as O
import qualified Synthesizer.Basic.Phase as P
import qualified Synthesizer.Basic.Wave as W
import qualified Synthesizer.Storable.Signal as S
import Midivis.Midi.MidiParser ( MidiEvent )
import Midivis.Util.Math
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word32, Word64)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
-- for debugging
import Numeric (showFFloat)

defPolyphony :: Int
defPolyphony = 128

defChunkSize :: S.ChunkSize
defChunkSize = S.chunkSize 64

defChunkSizeNum :: Double
defChunkSizeNum = 64

defSampleRate :: Double
defSampleRate = 96000.0

--------------------------------------------------------------------------------
-- Duration scaling: [0,1] ↔ [0.001s, 60s] via log10 mapping
--------------------------------------------------------------------------------
minDur, maxDur :: Float
minDur = 0.001
maxDur = 60.0

log10MinDur, log10DurRange :: Float
log10MinDur = logBase 10 minDur     -- -3.0
log10DurRange = logBase 10 maxDur - log10MinDur  -- ≈ 1.778 - (-3.0) = 4.778

-- | [0,1] normalized → seconds, log10 mapped
--   e.g. 0 → 0.001s (min), 0.5 → ~0.245s, 1 → 60s (max)
scaleDuration :: Float -> Float
scaleDuration n = 10 ** (log10MinDur + n * log10DurRange)

-- | seconds → [0,1] normalized, log10 mapped
unscaleDuration :: Float -> Float
unscaleDuration d = (logBase 10 d - log10MinDur) / log10DurRange

--------------------------------------------------------------------------------
-- Envelope state: compressed into a single Word64 (8 bytes)
--
-- Bit layout of the Word64:
--   bit 63-61: tag (3 bits, 0=Idle … 4=Release)
--   bit 60-32: param1 (29 bits, unsigned), fraction of [0,1]
--              Attack/Decay/Release → unscaleDuration⁻¹(duration)
--              Sustain             → level [0,1]
--   bit 31-0:  param2 (32 bits), raw Float bits (IEEE 754)
--              Attack/Decay/Release → curve [0,1] (0=convex, 0.5=linear, 1=concave)
--              Idle/Sustain         → 0
--------------------------------------------------------------------------------
data EnvelopeState
    = Idle
    | Attack  !Float !Float   -- (duration in seconds, curve [0,1])
    | Decay   !Float !Float   -- (duration in seconds, curve [0,1])
    | Sustain !Float          -- (level [0,1])
    | Release !Float !Float   -- (duration in seconds, curve [0,1])
    deriving (Eq)

instance Show EnvelopeState where
    show st = padRight 40 ' ' $ case st of
        Idle           -> "Idle"
        Attack d c     -> "Attack d=" ++ fmtF 8 3 d ++ " c=" ++ fmtF 4 3 c
        Decay d c      -> "Decay  d=" ++ fmtF 8 3 d ++ " c=" ++ fmtF 4 3 c
        Sustain l      -> "Sustain l=" ++ fmtF 6 3 l
        Release d c    -> "Release d=" ++ fmtF 8 3 d ++ " c=" ++ fmtF 4 3 c

instance Storable EnvelopeState where
    -- layout: Word64(8)
    sizeOf _    = 8
    alignment _ = 8

    peek ptr = do
        w <- peekByteOff ptr 0 :: IO Word64
        let tag         = (w `shiftR` 61) .&. 0x7
            param1Frac  = fromIntegral ((w `shiftR` 32) .&. 0x1FFFFFFF) / max29 :: Float
            param2      = castWord32ToFloat (fromIntegral (w .&. 0xFFFFFFFF) :: Word32)
        case tag of
            0 -> pure Idle
            1 -> pure $ Attack  (unscaleDuration param1Frac) param2
            2 -> pure $ Decay   (unscaleDuration param1Frac) param2
            3 -> pure $ Sustain param1Frac   -- level is already [0,1]
            4 -> pure $ Release (unscaleDuration param1Frac) param2
            _ -> pure Idle
      where
        max29 = 0x1FFFFFFF :: Float

    poke ptr = \case
        Idle           -> pokeByteOff ptr 0 (0 :: Word64)
        Attack  d c    -> pokeEnc 1 (scaleDuration d) (castFloatToWord32 c)
        Decay   d c    -> pokeEnc 2 (scaleDuration d) (castFloatToWord32 c)
        Sustain l      -> pokeEnc 3 l 0
        Release d c    -> pokeEnc 4 (scaleDuration d) (castFloatToWord32 c)
        where
            pokeEnc :: Word64 -> Float -> Word32 -> IO ()
            pokeEnc tag frac w32 =
                pokeByteOff ptr 0 $
                    (tag   `shiftL` 61)                .|.
                    (p1Rnd `shiftL` 32)                .|.
                    fromIntegral w32
                where
                    p1Rnd = fromIntegral (round (frac * 0x1FFFFFFF) :: Word64) .&. 0x1FFFFFFF

--------------------------------------------------------------------------------
-- A single synthesizer voice
--------------------------------------------------------------------------------
data VoiceParams = VoiceParams
    { vpFrequency      :: !Double           -- (Hz)
    , vpAmplitude      :: !Double           -- [0,1]
    , vpPhase          :: !Double           -- phase accumulator
    , vpEnvelopeState  :: !EnvelopeState    -- packed into 8 bytes
    }


instance Show VoiceParams where
    show (VoiceParams f a p e) =
        padRight 80 ' ' $
            "Freq=" ++ fmtD 10 3 f ++
            " Amp=" ++ fmtD 7 3 a ++
            " Phase=" ++ fmtD 10 3 p ++
            " Env=" ++ show e



fmtF :: Int -> Int -> Float -> String
fmtF _ d v = showFFloat (Just d) v ""

fmtD :: Int -> Int -> Double -> String
fmtD _ d v = showFFloat (Just d) v ""

padRight :: Int -> Char -> String -> String
padRight n ch s = take n (s ++ replicate n ch)

instance Storable VoiceParams where
    -- layout: Double(8) + Double(8) + Double(8) + EnvelopeState(8) = 32
    sizeOf _    = 32
    alignment _ = 8

    peek ptr = VoiceParams
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16
        <*> peekByteOff ptr 24

    poke ptr (VoiceParams f a p e) = do
        pokeByteOff ptr 0  f
        pokeByteOff ptr 8  a
        pokeByteOff ptr 16 p
        pokeByteOff ptr 24 e

defVoiceParams :: VoiceParams
defVoiceParams = VoiceParams 0xFFFFFFFF 0 0 Idle

defNormalizeAmp :: Double
defNormalizeAmp = fromDBFS (-9.0)    -- -9dBFS per voice

--------------------------------------------------------------------------------
-- Convolution reverb parameters
--------------------------------------------------------------------------------

-- | Path to the mono impulse-response WAV (converted from assets/audio/IR).
defConvIRPath :: FilePath
defConvIRPath = "assets/audio/IR/ir.wav"

-- | Fallback: if no IR file exists, use a synthetic decaying-noise IR of this
--   length (samples) and T60 (seconds).
defConvIRLen :: Int
defConvIRLen = 192000                -- 2 s @ 96 kHz

defConvIRT60 :: Double
defConvIRT60 = 1.8                   -- seconds

-- | Partition (block) size for the uniform partitioned convolution.
--   Must be a power of two; latency = block / sampleRate (~21 ms @ 96 kHz).
--   Note: total multiply-accumulate load = 2·IR·sr/N, so small blocks (e.g.
--   512) are ~4x heavier than 2048 and exceed the real-time budget in pure
--   Haskell, causing audio dropouts.
defConvBlockSize :: Int
defConvBlockSize = 2048

-- | Truncate the IR to this many seconds before convolving.  The reverb tail
--   beyond this is discarded, cutting the per-block multiply-accumulate work
--   proportionally (1.5 s of tail is plenty for most spaces).
defConvMaxTail :: Double
defConvMaxTail = 1.5                   -- seconds

-- | Mix levels: dry is the unprocessed signal, wet the convolved tail.
defConvDry :: Double
defConvDry = 0.8

defConvWet :: Double
defConvWet = 0.85

-- | Master gain applied after the dry/wet mix.
defConvGain :: Double
defConvGain = 1.2

--------------------------------------------------------------------------------
-- Random LFO (amplitude wobble on the two partials)
--------------------------------------------------------------------------------

-- | LFO 1 (fundamental): slow, shallow — small amplitude wobble.
defLFORate1 :: Double
defLFORate1 = 0.8                    -- Hz

-- | LFO 2 (overtone): faster, deeper — the 0.25·sin2 partial wobbles more.
defLFORate2 :: Double
defLFORate2 = 1.7                    -- Hz

defLFORate3 :: Double
defLFORate3 = 2.5

defLFODepth1 :: Double
defLFODepth1 = 0.1                  -- ±10%

defLFODepth2 :: Double
defLFODepth2 = 0.19                  -- ±19%

defLFODepth3 :: Double
defLFODepth3 = 0.56

defLFOSeed1 :: Int
defLFOSeed1 = 0xDEAD

defLFOSeed2 :: Int
defLFOSeed2 = 0xBEEF

defLFOSeed3 :: Int
defLFOSeed3 = 0xDAA7

--------------------------------------------------------------------------------
-- Amplitude smoothing (polyphonic aftertouch click prevention)
--------------------------------------------------------------------------------

-- | Second-order (two-stage cascade) smoothing coefficient per audio block.
--   Each stage moves this fraction of the way toward its target:
--       s1' = s1 + k·(target − s1)
--       s2' = s2 + k·(s1' − s2)      ← output
--   Two stages give 40 dB/dec roll-off (vs 20 dB/dec single-stage), so
--   aftertouch glides softly without clicks or steppy jumps.  k=0.1 gives
--   roughly a 100 ms two-stage time constant at 512-frame blocks.
defAmpSmoothK :: Double
defAmpSmoothK = 0.01

defAttack :: EnvelopeState
defAttack = Attack 0.1 0.3    -- curve -> 0 for "instant" on (10th root of x)

defDecay :: EnvelopeState
defDecay = Decay 0.9 0.6

defSustainLevel :: Float
defSustainLevel = 0.8

defSustainLevel' :: Double
defSustainLevel' = 0.8

defSustain :: EnvelopeState
defSustain = Sustain defSustainLevel

defRelease :: EnvelopeState
defRelease = Release 0.1 0.1

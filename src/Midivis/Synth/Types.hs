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
    deriving (Show, Eq)

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
    } deriving (Show)

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
defNormalizeAmp = fromDBFS (-12.0)    -- -12dBFS per voice

defAttack :: EnvelopeState
defAttack = Attack 0.1 0.3    -- curve -> 0 for "instant" on (10th root of x)

defDecay :: EnvelopeState
defDecay = Decay 0.3 0.5      -- 300ms decay, linear curve

defSustainLevel :: Float
defSustainLevel = 0.8

defSustainLevel' :: Double
defSustainLevel' = 0.8

defSustain :: EnvelopeState
defSustain = Sustain defSustainLevel

defRelease :: EnvelopeState
defRelease = Release 0.2 1  -- 500ms release, linear curve


{-
data SynthState = SynthState
    { ssVoices        :: !(SV.Vector VoiceParams)
    , ssMasterVolume  :: !Double
    , ssSampleRate    :: !Double
    , ssFrequencyTable :: !(SV.Vector Double)
    }

data AppState = AppState
    { appSynthState :: !(TVar SynthState)
    , appMidiQueue  :: !(TQueue MidiEvent)
    , appAudioBuffer :: !(TVar (SV.Vector Double))
    }
-}
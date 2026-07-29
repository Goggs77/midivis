{-# LANGUAGE LambdaCase #-}
module Midivis.Synth.Types where

import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector as SV
import qualified Synthesizer.Storable.Oscillator as O
import qualified Synthesizer.Basic.Phase as P
import qualified Synthesizer.Basic.Wave as W
import qualified Synthesizer.Storable.Signal as S
import Midivis.Midi.MidiParser ( MidiEvent )
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)
import Data.Word (Word32)

defPolyphony :: Int
defPolyphony = 128

defChunkSize :: S.ChunkSize
defChunkSize = S.chunkSize 64

defSampleRate :: Double
defSampleRate = 48000.0

defVoiceParams :: VoiceParams
defVoiceParams = VoiceParams 0xFFFFFFFF 0 0 Idle

defAttack :: EnvelopeState
defAttack = Attack 0.1

defDecay :: EnvelopeState
defDecay = Decay 0.3

defDecayAmp :: Double
defDecayAmp = 0.8

defRelease :: EnvelopeState
defRelease = Release 0.3

type VoicePool = SV.Vector VoiceParams

data VoiceParams = VoiceParams
    { vpFrequency   :: !Double           -- (Hz)
    , vpAmplitude   :: !Double           -- [0,1]
    , vpPhase       :: !Double           -- just phase
    , vpEnvelopeState :: !EnvelopeState -- env 
    } deriving (Show)

data EnvelopeState
    = Idle
    | Attack !Double   -- /
    | Decay !Double    --  \
    | Sustain         --   --    as long as midi input sustains
    | Release !Double  --     \
    deriving (Show, Eq)

instance Storable EnvelopeState where
    -- layout: Word32 tag(4) + pad(4) + Double(8) = 16
    sizeOf _    = 16
    alignment _ = 8  -- alignment of Double

    peek ptr = do
        tag <- peekByteOff ptr 0 :: IO Word32
        val <- peekByteOff ptr 8 :: IO Double
        pure $ case tag of
            0 -> Idle
            1 -> Attack val
            2 -> Decay val
            3 -> Sustain
            4 -> Release val
            _ -> Idle

    poke ptr = \case
        Idle      -> pokeTag 0 *> pokeVal 0
        Attack  x -> pokeTag 1 *> pokeVal x
        Decay   x -> pokeTag 2 *> pokeVal x
        Sustain   -> pokeTag 3 *> pokeVal 0
        Release x -> pokeTag 4 *> pokeVal x
      where
        pokeTag n = pokeByteOff ptr 0 (n :: Word32)
        pokeVal x = pokeByteOff ptr 8 (x :: Double)

instance Storable VoiceParams where
    -- layout: Double(8) + Double(8) + Double(8) + EnvelopeState(16) = 40
    sizeOf _    = 40
    alignment _ = 8  -- alignment of Double

    peek ptr = do
        freq <- peekByteOff ptr 0
        amp  <- peekByteOff ptr 8
        ph   <- peekByteOff ptr 16
        env  <- peekByteOff ptr 24
        pure $ VoiceParams freq amp ph env

    poke ptr (VoiceParams freq amp ph env) = do
        pokeByteOff ptr 0  freq
        pokeByteOff ptr 8  amp
        pokeByteOff ptr 16 ph
        pokeByteOff ptr 24 env

data SynthState = SynthState
    { ssVoices        :: !(SV.Vector VoiceParams)  -- 最多支持 polyphony 个声部
    , ssMasterVolume  :: !Double
    --, ssEffectsParams :: !EffectsParams
    , ssSampleRate    :: !Double
    , ssFrequencyTable :: !(SV.Vector Double)       -- freq table from .scl
    }

data AppState = AppState
    { appSynthState :: !(TVar SynthState)      -- 可被多个线程更新的合成状态
    , appMidiQueue  :: !(TQueue MidiEvent)     -- MIDI 事件队列
    , appAudioBuffer :: !(TVar (SV.Vector Double)) -- 预生成的音频块（双缓冲）
    }
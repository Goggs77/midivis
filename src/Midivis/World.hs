{-# OPTIONS_GHC -Wno-missing-fields #-}
module Midivis.World(World(..)
    , MidiEventBuffer
    , pack
    , unpack
    , initWorld
    , readWorld
    , readWorld'
    , writeWorld
    , writeWorld'
    , isFixedFrame
    , midiLength
    , TuningScroll(..)) where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable
import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector as SV
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.IORef (IORef, newIORef)
import qualified Data.Text as T

import Midivis.Midi.MidiParser (MidiEvent)
import Midivis.Resources.Tuning.All
import Midivis.Tuning.TuningParser
import Midivis.Tuning.Ascl (Ascl, calibrateA4)
import Midivis.Synth.VoicePool (VoicePool, newVoicePool)
import Midivis.Synth.CtrlBus (CtrlBus, newCtrlBus)
import Midivis.Synth.Preset (Voice, piano, allPresets, flute)
import Midivis.Synth.Render (SynthFx)

newtype MidiEventBuffer = MidiEventBuffer (Vector MidiEvent) 
instance Show MidiEventBuffer where show (MidiEventBuffer buf) = show $ V.toList buf
instance Semigroup MidiEventBuffer where (MidiEventBuffer a) <> (MidiEventBuffer b) =MidiEventBuffer $ a V.++ b 
instance Monoid MidiEventBuffer where mempty = MidiEventBuffer( V.fromList [] )

data TuningScroll = Next | Stop | Prev
    deriving (Eq, Show)

data World =  World 
    {
        -- timing
        time :: !Float,
        totalFrames :: !Int,
        initialized :: !Bool,
        -- gui stuff
        fixedFrameRate :: !Integer,
        fixedFrameCounter :: !Float,
        baseAngle :: !Double,
        clockVelocity :: !Double, 
        bufferVelocity :: !Double,
        bufferTransparency :: !Double,
        ampsum :: Double,
        sampleLRCache :: V.Vector Double,
        sampleLRsize :: Int,
        sampleLRindex :: Int,
        -- output RMS meter: raw block RMS (L, R) written by the audio
        -- callback into SynthFx.sfRmsRef; the Gloss step thread smooths it
        -- into these UI-owned fields each frame
        rmsSmL :: !Double,
        rmsSmR :: !Double,
        -- raw output sample (L, R) in [-1,1], synced from SynthFx.sfSampleRef
        -- by the step thread each frame (per block, ~94 Hz — an undersampled
        -- waveform point for scope/trace rendering)
        sampleL :: !Double,
        sampleR :: !Double,
        -- midi and tuning
        midiEvtBuf :: !MidiEventBuffer,
        midiEvtCpy :: SV.Vector MidiEvent,
        midiEvtQue :: !(TQueue (SV.Vector MidiEvent)),
        sclIndex :: !Int,
        freqA4 :: !Double,
        tuningScroll :: TuningScroll,
        sclOfChoice :: !Ascl,
        keyboardShift :: !Int,
        chordName :: T.Text,
        -- runtime synth resources (created once at initWorld; the engine and
        -- the audio callback hold the same references)
        voicePool :: !VoicePool,
        ctrlBus :: !CtrlBus,
        -- | Global sample counter: the audio callback advances it by the
        --   block length every block; the engine reads it on NoteOn to stamp
        --   the voice's onset (notelen mod source).
        sampleCounter :: !(IORef Double),
        -- | Active preset selection (',' / '.' scroll; Engine reads the
        --   voice from here, so a switch takes effect on the next event).
        presetIndex :: !Int,
        presetScroll :: TuningScroll,
        activeVoice :: !Voice,
        -- | The audio callback's effect/state bundle, published by Main once
        --   the audio thread is up (preset switch pushes new parameters in).
        synthFxRef :: !(IORef (Maybe SynthFx))
    }

-- | Create the full world state, including the runtime synth resources
--   (VoicePool + CtrlBus).  The engine thread reads them back from the TVar;
--   the PortAudio callback captures the same references at startup (the
--   real-time thread must never touch STM).
initWorld :: TQueue (SVB.Vector MidiEvent) -> IO World
initWorld tq = do
    vp <- newVoicePool
    bus <- newCtrlBus
    sc <- newIORef (0.0 :: Double)
    fxr <- newIORef Nothing
    return World {
    -- timing
    time = 0.0, 
    totalFrames = 0,
    initialized = False,
    -- gui stuff
    fixedFrameRate = 144, 
    fixedFrameCounter = 0.0, 
    baseAngle = 0.0, 
    clockVelocity = pi / 288 / 1.5,
    bufferVelocity = 0,
    bufferTransparency = 0,
    ampsum = 0,
    rmsSmL = 0,
    rmsSmR = 0,
    sampleL = 0,
    sampleR = 0,
    sampleLRCache = V.replicate 1440 0.0, --sync change with size
    sampleLRsize = 1440,
    sampleLRindex = 0,
    -- midi and tuning
    midiEvtBuf = mempty,
    midiEvtCpy = SV.empty,
    midiEvtQue = tq,
    sclIndex = 10,
    freqA4 = 440,
    tuningScroll = Stop,
    sclOfChoice = calibrateA4 (quintal12) 440,
    keyboardShift = 0,
    chordName = T.pack "",
    -- synth
    voicePool = vp,
    ctrlBus = bus,
    sampleCounter = sc,
    -- presets
    presetIndex = 0,
    presetScroll = Stop,
    activeVoice = piano,
    synthFxRef = fxr
    }

-- | Writes the World atomically
writeWorld :: TVar World -> World -> IO ()
writeWorld w0TVar w0 = do
    _ <- atomically $ writeTVar w0TVar w0 -- discard return STM ()
    return ()

-- | Writes the World atomically and return input, used with >>= chains
writeWorld' :: TVar World -> World -> IO World
writeWorld' w0TVar w0 = do
    _ <- atomically $ writeTVar w0TVar w0 -- discard return STM ()
    return w0

-- | Reads the World atomically and returns it
readWorld :: TVar World -> IO World
readWorld w0TVar = do
    w1 <- readTVarIO w0TVar
    return w1

-- | Reads the World atomically and returns it, used with >>= chains
readWorld' :: TVar World -> World -> IO World
readWorld' w0TVar _ = do
    w1 <- readTVarIO w0TVar
    return w1


isFixedFrame :: World -> Bool
isFixedFrame w0 = fixedFrameCounter w0 == 0

unpack :: MidiEventBuffer -> Vector MidiEvent
unpack (MidiEventBuffer v) = v

pack :: Vector MidiEvent -> MidiEventBuffer
pack = MidiEventBuffer

midiLength :: World -> Integer
midiLength w0 = fromIntegral $ V.length . unpack $ midiEvtBuf w0
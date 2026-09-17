module Midivis.Synth.VoicePool where

import Midivis.Synth.Types
import Foreign.Storable (peek, poke, peekByteOff, pokeByteOff)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Control.Monad (forM_)

-- | Fixed-size, pinned-memory pool of 128 voices (128 * 72 = 9216 bytes).
--   Two threads can read/write non-overlapping fields without locks:
--     MIDI thread:  vpFrequency (offset 0), vpEnvelopeState (offset 24),
--                   vpEnv2State (offset 32), vpEnv3State (offset 48),
--                   vpOnset (offset 64)
--     Synth thread: vpAmplitude (offset 8), vpPhase (offset 16),
--                   vpEnv2Amp (offset 40), vpEnv3Amp (offset 56)
--   Envelope states are single Word64 writes on each side;
--   mild races on envelope transitions are acceptable for real-time use.
newtype VoicePool = VoicePool (ForeignPtr VoiceParams)

-- | Allocate and initialise all 128 slots to defVoiceParams (freq=0xFFFFFFFF, Idle).
newVoicePool :: IO VoicePool
newVoicePool = do
    fp <- mallocForeignPtrBytes (defPolyphony * sizeOfVoiceParams)
    withForeignPtr fp $ \base ->
        forM_ [0 .. defPolyphony - 1] $ \i ->
            poke (base `plusPtr` (i * sizeOfVoiceParams)) defVoiceParams
    return $ VoicePool fp
    where
        sizeOfVoiceParams = 72 -- bytes

-- | Run an IO action with a raw pointer to slot @i@.
withSlot :: VoicePool -> Int -> (Ptr VoiceParams -> IO a) -> IO a
withSlot (VoicePool fp) i action =
    withForeignPtr fp $ \base ->
        action (base `plusPtr` (i * 72))

--------------------------------------------------------------------------------
-- Field-level helpers — each is a single aligned write (≤8 bytes).
-- On x86_64, 8-byte aligned loads/stores are atomic in hardware.
--------------------------------------------------------------------------------

-- | vpFrequency: written by MIDI thread.
readFreq :: VoicePool -> Int -> IO Double
readFreq pool i = withSlot pool i $ \p -> peekByteOff p 0

writeFreq :: VoicePool -> Int -> Double -> IO ()
writeFreq pool i v = withSlot pool i $ \p -> pokeByteOff p 0 v

-- | vpAmplitude: written by synth thread.
readAmp :: VoicePool -> Int -> IO Double
readAmp pool i = withSlot pool i $ \p -> peekByteOff p 8

writeAmp :: VoicePool -> Int -> Double -> IO ()
writeAmp pool i v = withSlot pool i $ \p -> pokeByteOff p 8 v

-- | vpPhase: written by synth thread.
readPhase :: VoicePool -> Int -> IO Double
readPhase pool i = withSlot pool i $ \p -> peekByteOff p 16

writePhase :: VoicePool -> Int -> Double -> IO ()
writePhase pool i v = withSlot pool i $ \p -> pokeByteOff p 16 v

-- | vpEnvelopeState (Word64): written by both threads. But areas don't overlap.
readEnv :: VoicePool -> Int -> IO EnvelopeState
readEnv pool i = withSlot pool i $ \p -> peekByteOff p 24

writeEnv :: VoicePool -> Int -> EnvelopeState -> IO ()
writeEnv pool i v = withSlot pool i $ \p -> pokeByteOff p 24 v

-- | vpEnv2State (Word64): ENV2 envelope (mod source).  Both threads.
readEnv2 :: VoicePool -> Int -> IO EnvelopeState
readEnv2 pool i = withSlot pool i $ \p -> peekByteOff p 32

writeEnv2 :: VoicePool -> Int -> EnvelopeState -> IO ()
writeEnv2 pool i v = withSlot pool i $ \p -> pokeByteOff p 32 v

-- | vpEnv2Amp: ENV2 output level, written by synth thread, read as a mod source.
readEnv2Amp :: VoicePool -> Int -> IO Double
readEnv2Amp pool i = withSlot pool i $ \p -> peekByteOff p 40

-- | vpEnv3State (Word64): ENV3 envelope (mod source).  Both threads.
readEnv3 :: VoicePool -> Int -> IO EnvelopeState
readEnv3 pool i = withSlot pool i $ \p -> peekByteOff p 48

writeEnv3 :: VoicePool -> Int -> EnvelopeState -> IO ()
writeEnv3 pool i v = withSlot pool i $ \p -> pokeByteOff p 48 v

-- | vpEnv3Amp: ENV3 output level, written by synth thread, read as a mod source.
readEnv3Amp :: VoicePool -> Int -> IO Double
readEnv3Amp pool i = withSlot pool i $ \p -> peekByteOff p 56

-- | vpOnset: global sample counter at NoteOn, written by MIDI thread.
--   The audio callback derives note age = (current sample counter − onset)/sr.
readOnset :: VoicePool -> Int -> IO Double
readOnset pool i = withSlot pool i $ \p -> peekByteOff p 64

writeOnset :: VoicePool -> Int -> Double -> IO ()
writeOnset pool i v = withSlot pool i $ \p -> pokeByteOff p 64 v

--------------------------------------------------------------------------------
-- Full-slot operations
--------------------------------------------------------------------------------

-- | Snapshot all fields of a voice slot (72 bytes, non-atomic read).
--   Used once per block per slot by the audio callback.  Each field write is
--   ≤8 bytes (atomic on x86_64), so a torn read only mixes fields across a
--   transition — phase/envelope stay continuous, no click results.
readVoice :: VoicePool -> Int -> IO VoiceParams
readVoice pool i = withSlot pool i peek

-- | Write all fields in one 72-byte poke (used for setup only).
writeVoice :: VoicePool -> Int -> VoiceParams -> IO ()
writeVoice pool i v = withSlot pool i $ \p -> poke p v

--------------------------------------------------------------------------------
--  Utilities
--------------------------------------------------------------------------------

-- | Find the first slot whose vpFrequency == 0xFFFFFFFF (the Idle sentinel).
--   Returns Nothing if all slots are occupied.
scanIdle :: VoicePool -> IO (Maybe Int)
scanIdle pool = go 0
    where
        go i
            | i >= defPolyphony = pure Nothing
            | otherwise = do
                freq <- readFreq pool i
                if freq == freqIdleSentinel
                    then pure (Just i)
                    else go (i + 1)

freqIdleSentinel :: Double
freqIdleSentinel = 0xFFFFFFFF

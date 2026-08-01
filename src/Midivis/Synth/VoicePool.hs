module Midivis.Synth.VoicePool where

import Midivis.Synth.Types
import Foreign.Storable (peek, poke, peekByteOff, pokeByteOff)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Control.Monad (forM_)

-- | Fixed-size, pinned-memory pool of 128 voices (128 * 32 = 4096 bytes).
--   Two threads can read/write non-overlapping fields without locks:
--     MIDI thread:  vpFrequency (offset 0), vpEnvelopeState (offset 24)
--     Synth thread: vpAmplitude (offset 8), vpPhase (offset 16)
--   vpEnvelopeState is a single Word64 write on each side;
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
        sizeOfVoiceParams = 32 -- bytes

-- | Run an IO action with a raw pointer to slot @i@.
withSlot :: VoicePool -> Int -> (Ptr VoiceParams -> IO a) -> IO a
withSlot (VoicePool fp) i action =
    withForeignPtr fp $ \base ->
        action (base `plusPtr` (i * 32))

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

--------------------------------------------------------------------------------
-- Full-slot operations
--------------------------------------------------------------------------------

-- | Snapshot all four fields of a voice slot (32 bytes, non-atomic read).
--   Used once per block per slot by the audio callback.  Each field write is
--   ≤8 bytes (atomic on x86_64), so a torn read only mixes fields across a
--   transition — phase/envelope stay continuous, no click results.
readVoice :: VoicePool -> Int -> IO VoiceParams
readVoice pool i = withSlot pool i peek

-- | Write all four fields in one 32-byte poke (used for setup only).
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

{-# LANGUAGE BangPatterns #-}
-- | Continuous-memory MIDI control bus.
--
--   A fixed, unboxed array of 0-127 values addressed by slot:
--       slots 0..127        → CC 0..127 (one slot per MIDI control-change)
--       slot  128           → velocity (most recent note-on)
--       slot  129           → aftertouch (channel pressure, last value)
--       slot  130           → pitch bend (compressed to 0-127, centre 64)
--       slots 131..135      → reserved for future special slots
--
--   Threading model: the MIDI thread WRITES (writeCC / writeSpecial); the
--   synth (audio callback) only READS (readCC / ccNorm / ...).  Values are
--   single bytes, so aligned loads/stores are atomic on x86_64 — readers
--   never see torn values and no locks are needed, exactly like VoicePool.
--   Keep the read path allocation-free: unsafeRead/unsafeWrite only.
module Midivis.Synth.CtrlBus
    ( CtrlBus
    , newCtrlBus
      -- slot addressing
    , numCcSlots, numSpecialSlots, numSlots
    , ccSlot, velSlot, aftertouchSlot, pitchBendSlot
      -- writers — MIDI thread only
    , writeCC, writeSpecial
      -- readers — synth thread (read-only view of the bus)
    , readCC, readSpecial, ccNorm, specialNorm
    ) where

import Prelude
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.Word (Word8)

-- | Opaque handle; the underlying storage is a single contiguous unboxed
--   array (no constructor exported — the bus can only be touched via the
--   helpers below).
newtype CtrlBus = CtrlBus (IOUArray Int Word8)

-- | Allocate the bus, zeroed.  Called once at startup; the array is mutable
--   and shared between the MIDI and synth threads from then on.
newCtrlBus :: IO CtrlBus
newCtrlBus = CtrlBus <$> newArray (0, numSlots - 1) 0

-- | 128 CC slots (CC 0..127), one per MIDI control-change number.
numCcSlots :: Int
numCcSlots = 128

-- | Special slots after the CCs: velocity, aftertouch, pitch bend + reserved.
numSpecialSlots :: Int
numSpecialSlots = 8

-- | Total slot count.
numSlots :: Int
numSlots = numCcSlots + numSpecialSlots

-- | Slot index of CC number @cc@ (0..127 — identity, kept as a function so
--   the layout rule lives in one place).
ccSlot :: Int -> Int
ccSlot = id

velSlot :: Int
velSlot = 128

aftertouchSlot :: Int
aftertouchSlot = 129

pitchBendSlot :: Int
pitchBendSlot = 130

clampByte :: Int -> Int
clampByte = max 0 . min 127

clampSlot :: Int -> Int
clampSlot = max 0 . min (numSlots - 1)

-- | Special-slot access takes ABSOLUTE slot numbers (128..135); clamping
--   with the CC range (0..127) or the global slot range would either hit the
--   CC region or index past the array (segfault).
clampSpecialAbs :: Int -> Int
clampSpecialAbs = max numCcSlots . min (numSlots - 1)

--------------------------------------------------------------------------------
-- Writers (MIDI thread)
--------------------------------------------------------------------------------

-- | Write a 0-127 value to CC slot @cc@ (out-of-range values clamp).
writeCC :: CtrlBus -> Int -> Int -> IO ()
writeCC (CtrlBus a) cc v =
    unsafeWrite a (ccSlot (clampSlot cc)) (fromIntegral (clampByte v))

-- | Write a 0-127 value to a special slot.  @slot@ is the ABSOLUTE slot
--   number — use the exported constants (velSlot=128, aftertouchSlot=129,
--   pitchBendSlot=130, ...).  Out-of-range slot/value clamp.
writeSpecial :: CtrlBus -> Int -> Int -> IO ()
writeSpecial (CtrlBus a) s v =
    unsafeWrite a (clampSpecialAbs s) (fromIntegral (clampByte v))

--------------------------------------------------------------------------------
-- Readers (synth thread)
--------------------------------------------------------------------------------

-- | Raw 0-127 value of CC slot @cc@.
readCC :: CtrlBus -> Int -> IO Int
readCC (CtrlBus a) cc =
    fromIntegral <$> unsafeRead a (ccSlot (clampSlot cc))

-- | Raw 0-127 value of a special slot — absolute slot number, e.g.
--   @readSpecial bus velSlot@.
readSpecial :: CtrlBus -> Int -> IO Int
readSpecial (CtrlBus a) s =
    fromIntegral <$> unsafeRead a (clampSpecialAbs s)

-- | Normalised [0,1] reading of a CC slot (v/127) — the convenient form for
--   driving modulation amounts directly.
ccNorm :: CtrlBus -> Int -> IO Double
ccNorm bus cc = (/ 127.0) . fromIntegral <$> readCC bus cc

-- | Normalised [0,1] reading of a special slot.
specialNorm :: CtrlBus -> Int -> IO Double
specialNorm bus s = (/ 127.0) . fromIntegral <$> readSpecial bus s

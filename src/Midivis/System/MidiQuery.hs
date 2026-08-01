{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Midivis.System.MidiQuery where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import qualified Data.StorableVector.Base as SV
import qualified Data.HashSet as H
import Data.Hashable
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Function (on)
import Sound.RtMidi

import Midivis.Midi.MidiParser
import Midivis.Midi.MidiEventType
import Midivis.World
import Control.Concurrent.STM (writeTQueue, atomically)
import System.IO (hPutStrLn, stderr)


-- Helpers to manipulate the global MidiEventBuffer
appendMidiEvent :: MidiEvent -> World -> IO World
appendMidiEvent e w0 = return w0 {midiEvtBuf = midiEvtBuf w0 <> pack (V.fromList [e])}

appendMidiEvents :: Vector MidiEvent -> World -> IO World
appendMidiEvents vs w0 = return w0 {midiEvtBuf = midiEvtBuf w0 <> pack vs}

getMidiBuffer :: World -> MidiEventBuffer
getMidiBuffer w0 = midiEvtBuf w0

clearMidiBuffer :: World -> IO World
clearMidiBuffer w0 = return w0 {midiEvtBuf = mempty}


-- | Initialize midi, don't block thread
initMidi :: IO (InputDevice)
initMidi  = do
    i <- defaultInput
    openPort i 0 "Midivis"
    return (i)

-- | Fill in MidiEventBuffer with MidiEvent(s), also avoid thread blocking operations
bufferMidi :: InputDevice -> World -> IO World
bufferMidi inputDevice w0 = do
    --The delta::Double in getMessage's return tuple represents
    -- the time elapsed in seconds since the previous MIDI message was received.
    (delta, msg) <- getMessageSized inputDevice 32
    --when queue is empty, delta == 0.0 and msg is empty vector
    --not using callback so there's no warning and empty return
    let mEvt = parse msg
    case mEvt of
        Nothing -> return w0 -- suppresse
        Just e  -> appendMidiEvent e w0

-- | Clean up MidiEventBuffer, also avoid thread blocking operations and speed-up calculations
--   Runs every frame (1 kHz).  Sorts events by noteId, pairs NoteOffs against
--   held NoteOns, associates aftertouch/PB with their notes, and ships the
--   result to the engine queue.  Only the active NoteOns are kept in the
--   buffer for the next frame's held-note matching.
cleanBuffer :: World -> IO World
cleanBuffer w0 = do
    let buf = (unpack $ midiEvtBuf w0)
    -- Idle fast path: no events at all — skip the whole pipeline
    -- (sorting / hashing / V.force / writeTQueue) which would otherwise
    -- allocate ~300B per frame at 1000 Hz even when nothing is happening.
    if V.null buf
        then return w0
        else do
            -- 1. split by event type, NoteOns/NoteOffs sorted by noteId
            let noteOns  = sortBy valueL $ V.filter (\e -> evt e == NoteOn) buf
                noteOffs = sortBy valueL $ V.filter (\e -> evt e == NoteOff) buf
                notePpA  = V.filter (\e -> evt e == PolyphonicAftertouch) buf
                notePB   = V.filter (\e -> evt e == PitchBendChange) buf
            -- MiniLab 3's Shift+Control triggers this benignly; just skip the frame
            if V.length noteOns < V.length noteOffs
                then do
                    hPutStrLn stderr "[Midi] Warning: discarding unbalanced frame (NoteOn < NoteOff)"
                    return w0{midiEvtBuf = mempty}
                else do
                    -- 2. pair NoteOffs with held NoteOns, keep aftertouch/PB
                    --    only for notes that are actually sounding
                    let processedOns = filterSortedByKey valueL noteOffs noteOns
                        processedPpA = takeSameIdWith valueL processedOns notePpA
                        processedPB  = takeSameIdWith valueL processedOns notePB
                        processedAll = V.force $ processedOns V.++ processedPpA V.++ processedPB
                        sv = copyElements processedAll
                    -- 3. ship to the engine (it blocks on this queue)
                    sv `seq` atomically $ writeTQueue (midiEvtQue w0) sv
                    -- Keep only the active NoteOns in the buffer for the next
                    -- frame's held-note matching.  Aftertouch/PB are consumed
                    -- here (sent to the queue) and dropped — otherwise they
                    -- would accumulate forever while a key is held, growing
                    -- the per-frame work linearly and causing audio dropouts.
                    return w0{
                        midiEvtBuf = pack $ processedOns,
                        midiEvtCpy = sv
                    }
        

copyElements :: (V.Storable a ) => V.Vector a -> SV.Vector a
copyElements buf = SV.SV fp offset len where
            (fp, offset, len) = V.unsafeToForeignPtr buf

-- | Removes ys' elements that has the same extractKey in xs, better when unsorted
dropSameIdWith :: (Hashable k, Eq k, V.Storable a, V.Storable k) => (a -> k) -> Vector a -> Vector a -> Vector a
dropSameIdWith extractKey xs ys =
    let forbiddenKeys = H.fromList (V.toList $ V.map extractKey xs)
    in V.filter (not . (`H.member` forbiddenKeys) . extractKey) ys
-- | Keeps ys' elements that has the same extractKey in xs, better when unsorted
takeSameIdWith :: (Hashable k, Eq k, V.Storable a, V.Storable k) => (a -> k) -> Vector a -> Vector a -> Vector a
takeSameIdWith extractKey xs ys =
    let allowedKeys = H.fromList (V.toList $ V.map extractKey xs)
    in V.filter ((`H.member` allowedKeys) . extractKey) ys

-- | In-place Introsort, 0 memory overhead
sortBy :: (V.Storable a, Ord b) => (a -> b) -> Vector a -> Vector a
sortBy extractKey = V.modify (Intro.sortBy (compare `on` extractKey))

-- | (Only when sorted in ascent order) ys \ xs => Removes ys' elements that has the same key in xs
filterSortedByKey ::(V.Storable a) => (a -> Int) -> Vector a -> Vector a -> Vector a
filterSortedByKey extractKey xs ys = V.unfoldr step (0, 0)
  where
    lenX = V.length xs
    lenY = V.length ys
    
    step (i, j) -- dual pointer merge
        | j >= lenY = Nothing                                 -- y out of bound, filter is complete
        | i >= lenX = Just (ys `V.unsafeIndex` j, (i, j + 1)) -- x out of bound, filter is complete
        | otherwise =
            let kx = extractKey (xs `V.unsafeIndex` i)        
                ky = extractKey (ys `V.unsafeIndex` j)        
            in case compare kx ky of
                LT -> step (i + 1, j)                         -- x<y, so filter y
                EQ -> step (i, j + 1)                         -- x==y, still filter but we need to jump to next y
                GT -> Just (ys `V.unsafeIndex` j, (i, j + 1)) -- x>y, we can keep this y and jump to next y
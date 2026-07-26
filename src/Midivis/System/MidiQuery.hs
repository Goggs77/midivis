{-# LANGUAGE BlockArguments #-}
module Midivis.System.MidiQuery where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import qualified Data.HashSet as H
import Data.Hashable
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Function (on)
import Sound.RtMidi

import Midivis.Midi.MidiParser
import Midivis.Midi.MidiEventType
import Midivis.World


-- Helpers to manipulate the global MidiEventBuffer
appendMidiEvent :: MidiEvent -> World -> IO World
appendMidiEvent e w0 = return w0 {midiEvtBuf = midiEvtBuf w0 <> pack (V.fromList [e])}

appendMidiEvents :: Vector MidiEvent -> World -> IO World
appendMidiEvents vs w0 = return w0 {midiEvtBuf = midiEvtBuf w0 <> pack vs}

getMidiBuffer :: World -> MidiEventBuffer
getMidiBuffer w0 = midiEvtBuf w0

clearMidiBuffer :: World -> IO World
clearMidiBuffer w0 = return w0 {midiEvtBuf = mempty}

forever :: Monad m => m a -> m b
forever a = a >> forever a



initMidi :: IO (InputDevice)
initMidi  = do
    --_ <- forkIO $ forever $ do
    --    (delta, msg) <- readChan chan 
    --    putStrLn $ "[Debug] " ++ show delta ++ " " ++ show msg
    i <- defaultInput
    openPort i 0 "Midivis"
    return (i)

-- feed it readChan chan when used
bufferMidi :: InputDevice -> World -> IO World
bufferMidi inputDevice w0 = do
    --The delta::Double in getMessage's return tuple represents
    -- the time elapsed in seconds since the previous MIDI message was received.
    (delta, msg) <- getMessageSized inputDevice 32
    --delta == 0.0 and msg is empty vector <- when queue is empty
    --not using callback so there's no warning and empty return
    {-putStrLn $ "[Debug] Parsing midi, delta: " ++ show delta-}
    let mEvt = parse msg
    case mEvt of
        Nothing -> {-(putStrLn "[Debug] Nothing to append") >>-} return w0
        Just e  -> {-(putStrLn $ "[Debug] Appended " ++ show e) >>-} appendMidiEvent e w0

cleanBuffer :: World -> IO World
cleanBuffer w0 = do
    let buf = (unpack $ midiEvtBuf w0)
        noteOns  = sortBy valueL $ V.filter (\e -> evt e == NoteOn) buf
        noteOffs = sortBy valueL $ V.filter (\e -> evt e == NoteOff) buf
        notePpA  = V.filter (\e -> evt e == PolyphonicAftertouch) buf
        notePB   = V.filter (\e -> evt e == PitchBendChange) buf
        processedOns = filterSortedByKey valueL noteOffs noteOns
        processedPpA = takeSameIdWith valueL processedOns notePpA
        processedPB  = takeSameIdWith valueL processedOns notePB
    {-putStrLn $ "[Debug] Original Buffer: " ++ show buf
    putStrLn $ "[Debug] Original Ons: " ++ show noteOns
    putStrLn $ "[Debug] Original Offs: " ++ show noteOffs-}
    if (V.length noteOns >= V.length noteOffs) 
        then
            putStrLn ("[Debug] Cleaned Buffer: " ++ show (processedOns V.++ processedPpA V.++ processedPB)) >>
            return w0{midiEvtBuf = pack $  (processedOns V.++ processedPpA V.++ processedPB)} 
        -- all NoteOffs must be eliminated, so no need to process, as the error below
        else error "Note On/Off mismatch" --Known error that this is triggered when using Shift+Control on MiniLab 3

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

-- | (Only when sorted in ascent order) Removes ys' elements that has the same key in xs
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
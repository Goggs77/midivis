{-# LANGUAGE BlockArguments #-}
module Midivis.System.MidiQuery where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)
import qualified Data.HashSet as H
import Data.Hashable
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
    let buf = V.toList (unpack $ midiEvtBuf w0)
        noteOns  = filter (\e -> evt e == NoteOn) buf
        noteOffs = filter (\e -> evt e == NoteOff) buf
        notePpA  = filter (\e -> evt e == PolyphonicAftertouch) buf
        notePB   = filter (\e -> evt e == PitchBendChange) buf
        processedOns = dropSameIdWith valueL noteOffs noteOns
        processedPpA = takeSameIdWith valueL processedOns notePpA
        processedPB  = takeSameIdWith valueL processedOns notePB
    {-putStrLn $ "[Debug] Original Buffer: " ++ show buf
    putStrLn $ "[Debug] Original Ons: " ++ show noteOns
    putStrLn $ "[Debug] Original Offs: " ++ show noteOffs-}
    if (length noteOns >= length noteOffs) 
        then
            putStrLn ("[Debug] Cleaned Buffer: " ++ show (processedOns ++ processedPpA ++ processedPB)) >>
            return w0{midiEvtBuf = pack $ V.fromList (processedOns ++ processedPpA ++ processedPB)} 
        -- all NoteOffs must be eliminated, so no need to process, as the error below
        else error "Note On/Off mismatch"


dropSameIdWith :: (Hashable k, Eq k) => (a -> k) -> [a] -> [a] -> [a]
dropSameIdWith extractKey xs ys =
    let forbiddenKeys = H.fromList (map extractKey xs)
    in filter (not . (`H.member` forbiddenKeys) . extractKey) ys

takeSameIdWith :: (Hashable k, Eq k) => (a -> k) -> [a] -> [a] -> [a]
takeSameIdWith extractKey xs ys =
    let forbiddenKeys = H.fromList (map extractKey xs)
    in filter ((`H.member` forbiddenKeys) . extractKey) ys
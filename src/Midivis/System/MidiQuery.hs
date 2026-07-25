{-# LANGUAGE BlockArguments #-}
module Midivis.System.MidiQuery where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector)

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
    (delta, msg) <- getMessage inputDevice
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
        processedOns = dropSameId noteOffs noteOns
        -- all NoteOffs must be eliminated, so no need to process, as the error below
    {-putStrLn $ "[Debug] Original Buffer: " ++ show buf
    putStrLn $ "[Debug] Original Ons: " ++ show noteOns
    putStrLn $ "[Debug] Original Offs: " ++ show noteOffs-}
    if (length noteOns >= length noteOffs) 
        then
            putStrLn ("[Debug] Cleaned Buffer: " ++ show processedOns) >>
            return w0{midiEvtBuf = pack $ V.fromList processedOns} 
        else error "Note On/Off mismatch"
    where
        dropSameId [] ys = ys
        dropSameId (x:xs) ys = 
            -- drop the first sameId element
            let newYs = (takeWhile (sameValueL x) ys) ++ (drop 1 $ dropWhile (sameValueL x) ys)
            in dropSameId xs newYs

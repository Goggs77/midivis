{-# LANGUAGE InstanceSigs #-}
module Midivis.Midi.MidiParser where
import Midivis.Midi.MidiEventType
import qualified Data.Vector.Storable as V
import Data.Vector.Storable ( fromList, Vector )
import Data.Word (Word8)
import Foreign.Storable
import Foreign.Ptr (castPtr)


data MidiEvent = MidiEvent 
    {
        channel::Int,
        evt::MidiEventType,
        valueL::Int, -- usually noteid, channel aftertouch pressure, or pitchbender LSB
        valueR::Int  -- usually velocity, value, polyphonic aftertouch pressure, or pitchbender MSB
    } deriving (Eq)
instance Show MidiEvent where 
    show (MidiEvent ch ev vL vR) = "MidiEvent {" ++ 
        show ev ++ "@chan: " ++ show ch ++ " w/ (L,R): (" ++ show vL ++ "," ++ show vR ++ ")}"

instance Storable MidiEvent where

    sizeOf :: MidiEvent -> Int
    sizeOf _ = sizeOf (0 :: Int) * 4

    alignment :: MidiEvent -> Int
    alignment _ = alignment (0 :: Int)

    peek ptr = do
        let intPtr = castPtr ptr
        ch <- peekElemOff intPtr 0
        ev <- peekElemOff intPtr 1
        vL <- peekElemOff intPtr 2
        vR <- peekElemOff intPtr 3
        return $ MidiEvent ch (toEnum ev) vL vR

    poke ptr (MidiEvent ch ev vL vR) = do
        let intPtr = castPtr ptr
        pokeElemOff intPtr 0 ch
        pokeElemOff intPtr 1 (fromEnum ev)
        pokeElemOff intPtr 2 vL
        pokeElemOff intPtr 3 vR

sameValueL :: MidiEvent -> MidiEvent -> Bool
sameValueL e1 e2 = (valueL e1) == (valueR e2)

-- Simply parse one msg
parse :: Vector Word8 -> Maybe MidiEvent
parse ws = parseThreeByteMessage ws 0

-- Parse a 3-byte MIDI message from a vector of bytes
-- Returns Just MidiEvent if parsing succeeds, Nothing if insufficient bytes or invalid message
parseThreeByteMessage :: Vector Word8 -> Int -> Maybe MidiEvent
parseThreeByteMessage bytes startIdx
    | startIdx + 2 >= V.length bytes = Nothing  -- Need at least 3 bytes
    | otherwise = 
        let
            statusByte = bytes V.! startIdx
            dataByte1 = bytes V.! (startIdx + 1)
            dataByte2 = bytes V.! (startIdx + 2)
            (chan, eventType) = toChannelEvent statusByte
        in
            -- Only parse if it's a valid 3-byte channel message for notes
            case eventType of
                NoteOff -> Just $ MidiEvent chan NoteOff (fromIntegral dataByte1) (fromIntegral dataByte2)
                NoteOn -> Just $ MidiEvent chan NoteOn (fromIntegral dataByte1) (fromIntegral dataByte2)
                PolyphonicAftertouch -> Just $ MidiEvent chan PolyphonicAftertouch (fromIntegral dataByte1) (fromIntegral dataByte2)
                ControlOrModeChange -> Just $ MidiEvent chan ControlOrModeChange (fromIntegral dataByte1) (fromIntegral dataByte2)
                PitchBendChange -> Just $ MidiEvent chan PitchBendChange (fromIntegral dataByte1) (fromIntegral dataByte2)
                _ -> Nothing  -- Not a 3-byte message

-- Parse all 3-byte MIDI messages from a vector of bytes
-- Advances by 3 bytes for each valid message found
parseThreeByteMessages :: Vector Word8 -> [MidiEvent]
parseThreeByteMessages bytes = go 0
  where
    len = V.length bytes
    go idx
        | idx + 2 >= len = []
        | otherwise = case parseThreeByteMessage bytes idx of
            Just event -> event : go (idx + 3)
            Nothing -> []  -- Stop parsing on first invalid message

-- Convert a MidiEvent back to 3 bytes (status byte, data1, data2)
toThreeBytes :: MidiEvent -> Vector Word8
toThreeBytes event =
    let
        statusByte = fromChannelEvent (channel event, evt event)
        data1 = fromIntegral (valueL event) :: Word8
        data2 = fromIntegral (valueR event) :: Word8
    in fromList [statusByte, data1, data2]


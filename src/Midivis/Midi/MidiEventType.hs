module Midivis.Midi.MidiEventType where
import Data.Word (Word8)
--Expanded MIDI 1.0 Messages

data MidiEventType = NoteOff -- [128 143]
                   | NoteOn -- [144 159]
                   | PolyphonicAftertouch -- [160 175]
                   | ControlOrModeChange -- [176 191]
                   | ProgramChange -- [192 207]
                   | ChannelAftertouch -- [208 223]
                   | PitchBendChange -- [224 239]
                   | SystemExclusive -- 240; System Exclusive (data dump) 2nd byte= Vendor ID (or Universal Exclusive) followed by more data bytes and ending with EOX.
                   | MIDITimeCodeQuarterFrame -- 241; see specs
                   | SongPositionPointer -- 242; L R
                   | SongSelect -- 243; 0-127, none
                   | Undefined244 -- occupied for testing
                   | Undefined245
                   | TuneRequest -- 246
                   | EOX -- 247
                   | TimingClock --248
                   | Undefined249
                   | Start -- 250
                   | Continue -- 251
                   | Stop -- 252
                   | Undefined253
                   | ActiveSensing -- 254
                   | SystemReset -- 255
                   deriving (Show, Eq, Enum)

toChannelEvent :: Word8 -> (Int, MidiEventType)
toChannelEvent wd =
    let
        i = fromIntegral wd :: Int --we just need the 8 bits in Word8
        evt = case i of
            n | 128 <= n && n <= 143 -> NoteOff
            n | 144 <= n && n <= 159 -> NoteOn
            n | 160 <= n && n <= 175 -> PolyphonicAftertouch
            n | 176 <= n && n <= 191 -> ControlOrModeChange
            n | 192 <= n && n <= 207 -> ProgramChange
            n | 208 <= n && n <= 223 -> ChannelAftertouch
            n | 224 <= n && n <= 239 -> PitchBendChange
            240 -> SystemExclusive
            241 -> MIDITimeCodeQuarterFrame
            242 -> SongPositionPointer
            243 -> SongSelect
            244 -> Undefined244
            245 -> Undefined245
            246 -> TuneRequest
            247 -> EOX
            248 -> TimingClock
            249 -> Undefined249
            250 -> Start
            251 -> Continue
            252 -> Stop
            253 -> Undefined253
            254 -> ActiveSensing
            255 -> SystemReset
            _ -> error "toChannelEvent: unknown MIDI status byte"
        chan = (i `mod` 16) + 1
        validNote = (128 <= i && i <= 239) -- this indicates if chan is meaningful
    in (if validNote then chan else -1, evt)

fromChannelEvent :: (Int, MidiEventType) -> Word8
fromChannelEvent (chan, ev) =
    let
        channelStatus base =
            if chan >= 1 && chan <= 16
            then fromIntegral (base + (chan - 1))
            else error "fromChannelEvent: invalid channel for channel event"
    in case ev of
        NoteOff -> channelStatus 128
        NoteOn -> channelStatus 144
        PolyphonicAftertouch -> channelStatus 160
        ControlOrModeChange -> channelStatus 176
        ProgramChange -> channelStatus 192
        ChannelAftertouch -> channelStatus 208
        PitchBendChange -> channelStatus 224
        SystemExclusive -> 240
        MIDITimeCodeQuarterFrame -> 241
        SongPositionPointer -> 242
        SongSelect -> 243
        Undefined244 -> 244
        Undefined245 -> 245
        TuneRequest -> 246
        EOX -> 247
        TimingClock -> 248
        Undefined249 -> 249
        Start -> 250
        Continue -> 251
        Stop -> 252
        Undefined253 -> 253
        ActiveSensing -> 254
        SystemReset -> 255
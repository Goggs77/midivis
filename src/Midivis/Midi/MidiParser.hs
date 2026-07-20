module Midivis.Midi.MidiParser where
import Sound.RtMidi
import Midivis.Midi.MidiEventType
import Data.Vector.Storable (Vector, (!))
import Data.Word (Word8)


data MidiEvent = MidiEvent 
    {
        channel::Int,
        evt::MidiEventType,
        valueL::Int, -- usually noteid, channel aftertouch pressure, or pitchbender LSB
        valueR::Int  -- usually velocity, value, polyphonic aftertouch pressure, or pitchbender MSB
    }
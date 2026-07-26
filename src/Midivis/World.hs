module Midivis.World(World(..), MidiEventBuffer, pack, unpack, initWorld) where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable

import Midivis.Midi.MidiParser (MidiEvent)
import Midivis.Resources.Tuning.All
import Midivis.Tuning.TuningParser

newtype MidiEventBuffer = MidiEventBuffer (Vector MidiEvent) 
instance Show MidiEventBuffer where show (MidiEventBuffer buf) = show $ V.toList buf
instance Semigroup MidiEventBuffer where (MidiEventBuffer a) <> (MidiEventBuffer b) =MidiEventBuffer $ a V.++ b 
instance Monoid MidiEventBuffer where mempty = MidiEventBuffer( V.fromList [] )

data World =  World 
    {
        midiEvtBuf :: MidiEventBuffer,
        sclOfChoice :: Scala,
        baseAngle :: Double
    }

initWorld :: World
initWorld = World {midiEvtBuf = mempty, baseAngle = 0.0, sclOfChoice = fiveSeven}

unpack :: MidiEventBuffer -> Vector MidiEvent
unpack (MidiEventBuffer v) = v

pack :: Vector MidiEvent -> MidiEventBuffer
pack = MidiEventBuffer
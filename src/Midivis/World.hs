module Midivis.World(World(..), MidiEventBuffer(..), initWorld) where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable
import Midivis.Midi.MidiParser (MidiEvent)

newtype MidiEventBuffer = MidiEventBuffer (Vector MidiEvent) 
instance Show MidiEventBuffer where show (MidiEventBuffer buf) = show $ V.toList buf
instance Semigroup MidiEventBuffer where (MidiEventBuffer a) <> (MidiEventBuffer b) =MidiEventBuffer $ a V.++ b 
instance Monoid MidiEventBuffer where mempty = MidiEventBuffer( V.fromList [] )

data World = World 
    {
        midiEvtBuf :: MidiEventBuffer
    }

initWorld :: World
initWorld = World {midiEvtBuf = mempty}
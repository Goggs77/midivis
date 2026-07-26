module Midivis.World(World(..), MidiEventBuffer, pack, unpack, initWorld, isFixedFrame, midiLength) where

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
        time :: Float,
        fixedFrameRate :: Integer,
        fixedFrameCounter :: Float,
        clockVelocity :: Double,
        bufferVelocity :: Double,
        midiEvtBuf :: MidiEventBuffer,
        sclOfChoice :: Scala,
        baseAngle :: Double
    }

initWorld :: World
initWorld = World {
    time = 0.0, 
    fixedFrameRate = 144, 
    fixedFrameCounter = 0.0, 
    clockVelocity = pi / 288 / 1.5,
    bufferVelocity = 0,
    midiEvtBuf = mempty, 
    baseAngle = 0.0, 
    sclOfChoice = edo12
    }

isFixedFrame :: World -> Bool
isFixedFrame w0 = fixedFrameCounter w0 == 0

unpack :: MidiEventBuffer -> Vector MidiEvent
unpack (MidiEventBuffer v) = v

pack :: Vector MidiEvent -> MidiEventBuffer
pack = MidiEventBuffer

midiLength :: World -> Integer
midiLength w0 = fromIntegral $ V.length . unpack $ midiEvtBuf w0
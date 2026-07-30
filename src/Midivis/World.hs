{-# OPTIONS_GHC -Wno-missing-fields #-}
module Midivis.World(World(..)
    , MidiEventBuffer
    , pack
    , unpack
    , initWorld
    , readWorld
    , readWorld'
    , writeWorld
    , writeWorld'
    , isFixedFrame
    , midiLength) where

import qualified Data.Vector.Storable as V
import Data.Vector.Storable
import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector as SV
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Midivis.Midi.MidiParser (MidiEvent)
import Midivis.Resources.Tuning.All
import Midivis.Tuning.TuningParser

newtype MidiEventBuffer = MidiEventBuffer (Vector MidiEvent) 
instance Show MidiEventBuffer where show (MidiEventBuffer buf) = show $ V.toList buf
instance Semigroup MidiEventBuffer where (MidiEventBuffer a) <> (MidiEventBuffer b) =MidiEventBuffer $ a V.++ b 
instance Monoid MidiEventBuffer where mempty = MidiEventBuffer( V.fromList [] )

data World =  World 
    {
        -- timing
        time :: Float,
        -- gui stuff
        fixedFrameRate :: Integer,
        fixedFrameCounter :: Float,
        baseAngle :: Double,
        clockVelocity :: Double, 
        bufferVelocity :: Double,
        -- midi and tuning
        midiEvtBuf :: MidiEventBuffer,
        midiEvtCpy :: SV.Vector MidiEvent,
        midiEvtQue :: !(TQueue (SV.Vector MidiEvent)),
        sclOfChoice :: Scala
    
    }

initWorld :: TQueue (SVB.Vector MidiEvent) -> World
initWorld tq = World {
    -- timing
    time = 0.0, 
    -- gui stuff
    fixedFrameRate = 144, 
    fixedFrameCounter = 0.0, 
    baseAngle = 0.0, 
    clockVelocity = pi / 288 / 1.5,
    bufferVelocity = 0,
    -- midi and tuning
    midiEvtBuf = mempty, 
      --midiEvtCpy = , just leave it, we'll update it once we have updated midi thread
    midiEvtQue = tq,
    sclOfChoice = fiveSeven
    }

-- | Writes the World atomically
writeWorld :: TVar World -> World -> IO ()
writeWorld w0TVar w0 = do
    _ <- atomically $ writeTVar w0TVar w0 -- discard return STM ()
    return ()

-- | Writes the World atomically and return input, used with >>= chains
writeWorld' :: TVar World -> World -> IO World
writeWorld' w0TVar w0 = do
    _ <- atomically $ writeTVar w0TVar w0 -- discard return STM ()
    return w0

-- | Reads the World atomically and returns it
readWorld :: TVar World -> IO World
readWorld w0TVar = do
    w1 <- readTVarIO w0TVar
    return w1

-- | Reads the World atomically and returns it, used with >>= chains
readWorld' :: TVar World -> World -> IO World
readWorld' w0TVar _ = do
    w1 <- readTVarIO w0TVar
    return w1


isFixedFrame :: World -> Bool
isFixedFrame w0 = fixedFrameCounter w0 == 0

unpack :: MidiEventBuffer -> Vector MidiEvent
unpack (MidiEventBuffer v) = v

pack :: Vector MidiEvent -> MidiEventBuffer
pack = MidiEventBuffer

midiLength :: World -> Integer
midiLength w0 = fromIntegral $ V.length . unpack $ midiEvtBuf w0
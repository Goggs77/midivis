module Midivis.Synth.Buffer where
import Control.Concurrent.STM
import Data.StorableVector as SV

newtype AudioBuffer = AudioBuffer (TVar (Vector Double))

newAudioBuffer :: Int -> IO AudioBuffer
newAudioBuffer n = AudioBuffer <$> newTVarIO (SV.replicate n 0.0)

pushBuffer :: AudioBuffer -> Vector Double -> IO ()
pushBuffer (AudioBuffer tv) chunk = atomically $ writeTVar tv chunk

pullBuffer :: AudioBuffer -> IO (Vector Double)
pullBuffer (AudioBuffer tv) = readTVarIO tv
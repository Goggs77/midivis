module Midivis.Synth.Engine where

import qualified Data.StorableVector.Base as SVB 
import qualified Data.StorableVector as SV
--- ^ actually the same type

import qualified Synthesizer.Storable.Oscillator as O
import qualified Synthesizer.Basic.Phase as P
import qualified Synthesizer.Basic.Wave as W
import qualified Synthesizer.Storable.Signal as S
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Foreign.Storable

import Midivis.Synth.Types 
import Midivis.World
import Midivis.Midi.MidiParser ( MidiEvent(..) )
import Midivis.Tuning.Scala
import Control.Monad (forM_)
import Midivis.Midi.MidiEventType (MidiEventType(..))
import Control.Concurrent (forkIO)

forever a = a >> forever a

-- w0TVar -> TQueue midi -> sampleRate -> chunkSize -> IO()
initSynth :: TVar World -> TQueue (SV.Vector MidiEvent) -> Double -> S.ChunkSize -> IO ()
initSynth w0TVar midiTQueue sr cks = do
    vpTVar <- newTVarIO $ SV.replicate defPolyphony defVoiceParams
    _ <- forkIO $ forever $ mainLoop w0TVar midiTQueue vpTVar
    error ""

-- all SVB.Vector MidiEvent(s) passed form TQueue are sorted, and filtered
-- leaving ordered NoteOn and PolyphonicAftertouch and PitchBendChange only
-- by this convention we can easily filter previous VoicePool by key freq.
-- ideally we need all scalas to also be sorted in ascent order, otherwise this would fail.
mainLoop :: TVar World -> TQueue (SV.Vector MidiEvent) -> TVar VoicePool -> IO ()
mainLoop w0TVar midiTQueue vpTVar = forever $ do
    midiEvts <- atomically $ readTQueue midiTQueue -- the blocking function
    preVP <-  readTVarIO vpTVar --preV -> previous
    w0 <- readWorld w0TVar
    let -- Generate NoteOn sub voice pool
        midigenVP = SV.unfoldr step 0 where
            len = SV.length midiEvts
            step i 
                | i >= len = Nothing
                | otherwise =
                    let e = midiEvts `SV.index` i
                    in case evt e of 
                        NoteOn -> Just (defVoiceParams{vpFrequency = getFreq (sclOfChoice w0) (valueL e)}, i+1)
                        _ -> step (i+1)
    let -- Manage new voice creation and basic envelope state updates
        -- the order matters here as it will retain other informations from preVP
        envOff = \vp -> vp {vpEnvelopeState = defRelease}
        envOn  = \vp -> vp {
            vpEnvelopeState =
                case vpEnvelopeState vp of
                    Idle -> defAttack
                    _ -> vpEnvelopeState vp
            }
        freqIdle = \vp -> vp {
            vpFrequency = 
                case vpEnvelopeState vp of
                    Idle -> 0xFFFFFFFF
                    _ -> vpFrequency vp
            }
        offs = SV.map envOff $ SV.filter (\vp -> vpAmplitude vp /= 0) $ filterSortedByKey (vpFrequency) midigenVP preVP
        ons = SV.map envOn $ SV.filter (\vp -> vpAmplitude vp /= 0) $ keepSortedByKey (vpFrequency) midigenVP preVP 
        rem = 128 - SV.length offs - SV.length ons -- assert rem == 0
    
    -- we should get another thread to process fine envelope state management and synthesis
    -- as this main loop will be blocked from time to time, all it can do would be sanity checks
    atomically $ writeTVar vpTVar $ SV.map freqIdle $ partialMergeSort (vpFrequency) ons offs


-- | (Only when sorted in ascent order) => ys \ xs, Removes ys' elements that has the same key in xs
filterSortedByKey :: (Storable a, Ord b) => (a -> b) -> SV.Vector a -> SV.Vector a -> SV.Vector a
filterSortedByKey extractKey xs ys = SV.unfoldr step (0, 0)
  where
    lenX = SV.length xs
    lenY = SV.length ys
    
    step (i, j) -- dual pointer merge
        | j >= lenY = Nothing                                 -- y out of bound, filter is complete
        | i >= lenX = Just (ys `SV.index` j, (i, j + 1)) -- x out of bound, filter is complete
        | otherwise =
            let kx = extractKey (xs `SV.index` i)        
                ky = extractKey (ys `SV.index` j)        
            in case compare kx ky of
                LT -> step (i + 1, j)                         -- x<y, so filter y
                EQ -> step (i, j + 1)                         -- x==y, still filter but we need to jump to next y
                GT -> Just (ys `SV.index` j, (i, j + 1)) -- x>y, we can keep this y and jump to next y

-- | (Only when sorted in ascent order) => ys ∩ xs, Keeps ys' elements that has the same key in xs
keepSortedByKey :: (Storable a, Ord b) => (a -> b) -> SV.Vector a -> SV.Vector a -> SV.Vector a
keepSortedByKey extractKey xs ys = SV.unfoldr step (0, 0)
  where
    lenX = SV.length xs
    lenY = SV.length ys
    
    step (i, j) -- dual pointer step
        | j >= lenY || i >= lenX = Nothing      --  out of bound == complete
        | otherwise =
            let kx = extractKey (xs `SV.index` i)        
                ky = extractKey (ys `SV.index` j)        
            in case compare kx ky of
                LT -> step (i + 1, j)                         -- x<y, don't have proof to keep y yet
                EQ -> Just (ys `SV.index` j, (i, j + 1))      -- x==y, still keep but we need to jump to next y
                GT -> step (i, j + 1)                         -- x>y, don't have proof to keep y yet but jump to next y

-- | (Only when sorted in ascent order) => merge ys xs
partialMergeSort :: (Storable a, Ord b) => (a -> b) -> SV.Vector a -> SV.Vector a -> SV.Vector a
partialMergeSort extractKey xs ys = SV.unfoldr step (0, 0, False)
  where
    lenX = SV.length xs
    lenY = SV.length ys
    
    step (i, j, b) -- dual pointer step + state
        | b         = Just (xs `SV.index` (i - 1), (i, j, False))    -- turn off flag and simply keep previous x
        | j >= lenY = Just (xs `SV.index` i, (i + 1, j, False))      -- y out of bound, sort is complete
        | i >= lenX = Just (ys `SV.index` j, (i, j + 1, False))      -- x out of bound, sort is complete
        | otherwise =
            let kx = extractKey (xs `SV.index` i)        
                ky = extractKey (ys `SV.index` j)        
            in case compare kx ky of
                LT -> Just (xs `SV.index` i, (i + 1, j, False))      -- x<y, keep x
                EQ -> Just (ys `SV.index` j, (i + 1, j + 1, True))   -- x==y, keep y and turn on flag to keep x the next turn
                GT -> Just (ys `SV.index` j, (i, j + 1, False))      -- x>y, keep y

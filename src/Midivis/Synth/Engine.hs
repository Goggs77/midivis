{-# LANGUAGE BangPatterns #-}
module Midivis.Synth.Engine where

import qualified Data.StorableVector as SV
import qualified Data.HashSet as H

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forM_, forever, when)

import Midivis.Synth.Types
import Midivis.Synth.VoicePool
import Midivis.World
import Midivis.Midi.MidiParser (MidiEvent(..))
import Midivis.Midi.MidiEventType (MidiEventType(..))
import System.Random (randomRIO)
import Midivis.Tuning.Scala (getFreq)

-- | Initialise the synth engine: create VoicePool, fork mainLoop.
--   Returns the VoicePool for PortAudio's callback thread to render from.
initSynth :: TVar World -> TQueue (SV.Vector MidiEvent) -> IO VoicePool
initSynth w0TVar midiTQueue = do
    vp <- newVoicePool
    _ <- forkIO $ mainLoop w0TVar midiTQueue vp
    return vp

-- | MIDI -> VoicePool bridge.
--   TQueue events are sorted by noteId and contain only NoteOn (held),
--   PolyphonicAftertouch, and PitchBendChange (NoteOffs already removed).
--
--   Mapping rule: slot[noteId] holds MIDI note @noteId@ (or Idle).
--   This gives O(1) write per event -- no sorting of the pool needed.
--
--   This thread blocks on readTQueue.  Fine-grained envelope timing
--   (Attack -> Decay -> Sustain -> Release -> Idle) is handled by the
--   PortAudio callback thread which reads/writes VoicePool directly.
mainLoop :: TVar World -> TQueue (SV.Vector MidiEvent) -> VoicePool -> IO ()
mainLoop w0TVar midiTQueue vp = forever $ do
    midiEvts <- atomically $ readTQueue midiTQueue
    w0 <- readWorld w0TVar
    let scala = sclOfChoice w0

    -- Phase 0: collect the set of noteIds currently held (repeated NoteOns
    -- for the same key arrive every frame while it is pressed).
    let heldSet = foldEvents H.empty midiEvts
          where
            foldEvents !acc evts =
                let len = SV.length evts
                    go i s
                        | i >= len  = s
                        | otherwise =
                            let e = evts `SV.index` i
                            in case evt e of
                                NoteOn    -> go (i + 1) (H.insert (valueL e) s)
                                _         -> go (i + 1) s
                in go 0 acc

    -- Phase 1: apply each incoming event to its noteId slot.
    forM_ [0 .. SV.length midiEvts - 1] $ \i -> do
        let e = midiEvts `SV.index` i
        case evt e of
            NoteOn -> do
                let noteId = valueL e
                    velocity = valueR e -- unused (fixed per-note gain)
                writeFreq vp noteId (getFreq scala noteId)
                env <- readEnv vp noteId
                -- Only retrigger Attack if voice is not already sustaining
                -- (Idle or Release).  cleanBuffer may send the same NoteOn
                -- repeatedly for held notes — we must NOT reset Sustaining
                -- voices back to Attack every ~1ms.
                case env of
                    Attack{} -> return ()
                    Decay{}  -> return ()
                    Sustain{} -> return ()
                    _ -> do
                        writeEnv vp noteId defAttack
                        -- Randomise the start phase on a fresh voice so
                        -- repeated notes don't hit the same waveform sample.
                        when (env == Idle) $
                            writePhase vp noteId =<< randomRIO (0.0, 1.0)
            PolyphonicAftertouch -> do
                let noteId = valueL e
                    amp    = fromIntegral (valueR e) / 127.0
                -- Write the target amp; the audio callback smooths it
                -- (two-stage cascade) so pressure glides instead of clicking.
                writeAmp vp noteId amp
            _ -> return ()

    -- Phase 2: release voices whose noteId is no longer being held.
    forM_ [0 .. 127] $ \slot -> do
        env <- readEnv vp slot
        case env of
            Idle -> return ()
            Release _ _-> return () -- don't write relase repeatedly
            _ | slot `H.member` heldSet -> return ()
              | otherwise               -> writeEnv vp slot defRelease

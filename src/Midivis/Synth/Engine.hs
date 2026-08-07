{-# LANGUAGE BangPatterns #-}
module Midivis.Synth.Engine where

import qualified Data.StorableVector as SV
import qualified Data.HashSet as H

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Control.Monad (forM_, forever, when)
import Data.IORef (newIORef, readIORef, writeIORef)

import Midivis.Synth.Types
import Midivis.Synth.VoicePool
import Midivis.World
import Midivis.Midi.MidiParser (MidiEvent(..))
import Midivis.Midi.MidiEventType (MidiEventType(..))
import System.Random (randomRIO)
import Midivis.Tuning.Ascl (Ascl, getFreq)
import Midivis.Synth.CtrlBus
    ( CtrlBus, writeCC, writeSpecial, velSlot, aftertouchSlot, pitchBendSlot )
import Midivis.Synth.Preset (Voice(..), EnvParams(..))
import Midivis.Synth.Types (EnvelopeState(..))

-- | Initialise the synth engine: read the VoicePool / CtrlBus / event queue
--   out of the world (created by 'initWorld') and fork mainLoop.
--   Returns nothing — the same VoicePool lives inside the World for the
--   PortAudio callback to capture.
initSynth :: TVar World -> Voice -> IO ()
initSynth w0TVar voice = do
    _ <- forkIO $ mainLoop w0TVar voice
    return ()

-- | Pitch-bend range in semitones (MIDI default: ±2; RPN 0 not tracked).
defPitchBendSemitones :: Double
defPitchBendSemitones = 2.0

-- | 14-bit bend value (0..16383, centre 8192) → frequency ratio.
--   centre 8192 → 1.0; ±8192 → ±defPitchBendSemitones semitones.
bendRatio :: Int -> Double
bendRatio pb14 = 2 ** (fromIntegral (pb14 - 8192) * defPitchBendSemitones / (8192.0 * 12.0))

-- | The consumer of PitchBendChange.  Re-runs the same getFreq-based
--   frequency computation as NoteOn (the "first frequency calculation"),
--   scaled by the current bend, for every sounding voice.  Idle slots are
--   skipped; Release voices keep their tail at the pre-bend pitch.
applyPitchBend :: VoicePool -> Ascl -> Int -> IO ()
applyPitchBend vp scala pb14 =
    forM_ [0 .. 127] $ \slot -> do
        env <- readEnv vp slot
        case env of
            Idle      -> return ()
            Release{} -> return ()
            _         -> writeFreq vp slot (bendRatio pb14 * getFreq scala slot)

-- | MIDI -> VoicePool / CtrlBus bridge.
--   TQueue events are sorted by noteId and contain NoteOn (held),
--   PolyphonicAftertouch, PitchBendChange, and ControlOrModeChange (CC)
--   (NoteOffs already removed).
--
--   Mapping rule: slot[noteId] holds MIDI note @noteId@ (or Idle).
--   This gives O(1) write per event -- no sorting of the pool needed.
--
--   This thread blocks on readTQueue.  Fine-grained envelope timing
--   (Attack -> Decay -> Sustain -> Release -> Idle) is handled by the
--   PortAudio callback thread which reads/writes VoicePool directly.
mainLoop :: TVar World -> Voice -> IO ()
mainLoop w0TVar voice = do
    -- | Current 14-bit pitch-bend (centre 8192), applied at the frequency
    --   computation point below.  Kept at full 14-bit precision here; the
    --   compressed 0-127 copy on the control bus is for other consumers.
    bendRef <- newIORef 8192
    forever $ do
      w0 <- readWorld w0TVar
      let midiTQueue = midiEvtQue w0
          vp         = voicePool w0
          bus        = ctrlBus w0
          scala      = sclOfChoice w0
          -- envelope trigger states from the active preset
          pE = vEnv voice
          !atkState = Attack (epAttack pE) (epAttackCurve pE)
          !relState = Release (epRelease pE) (epReleaseCurve pE)
      midiEvts <- atomically $ readTQueue midiTQueue

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
                      velocity = valueR e
                  -- track the most recent note-on velocity on the control bus
                  writeSpecial bus velSlot velocity
                  -- frequency = scale pitch × current pitch-bend ratio.
                  -- Bend applies at the SAME computation point as the note's
                  -- first frequency calculation.
                  pb <- readIORef bendRef
                  writeFreq vp noteId (bendRatio pb * getFreq scala noteId)
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
                          writeEnv vp noteId atkState
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
                  -- also expose the latest pressure on the control bus
                  writeSpecial bus aftertouchSlot (valueR e)
              ControlOrModeChange -> do
                  -- CC 0-127 → CtrlBus slot (valueL = CC number, valueR = value)
                  writeCC bus (valueL e) (valueR e)
              PitchBendChange -> do
                  -- 14-bit bend, centre 8192.  Two consumers:
                  --   1. compressed 0-127 copy on the control bus (for future
                  --      consumers / visualisation)
                  --   2. the real one: re-tune every sounding voice at the
                  --      same getFreq computation point as NoteOn
                  let pb14 = valueL e + valueR e * 128
                  writeIORef bendRef pb14
                  writeSpecial bus pitchBendSlot ((pb14 * 127 + 8191) `div` 16383)
                  applyPitchBend vp scala pb14
              _ -> return ()

      -- Phase 2: release voices whose noteId is no longer being held.
      forM_ [0 .. 127] $ \slot -> do
          env <- readEnv vp slot
          case env of
              Idle -> return ()
              Release _ _-> return () -- don't write relase repeatedly
              _ | slot `H.member` heldSet -> return ()
                | otherwise               -> writeEnv vp slot relState

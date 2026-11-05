module Midivis.EventHandler where
import Graphics.Gloss.Relative

import Midivis.World
import Midivis.System.MidiQuery (appendMidiEvent, clearMidiBuffer)
import Midivis.System.Selection
import Midivis.Midi.MidiParser (MidiEvent(..))
import Midivis.Midi.MidiEventType (MidiEventType(NoteOn, NoteOff))

-- | Computer-keyboard → MIDI note map, laid out to mirror the physical
--   keyboard: 4 rows × 5 keys, rows top→bottom (number row → qwert →
--   asdfg → zxcvb), each row left→right.  The notes are assigned walking
--   the keyboard bottom→top, left→right, stepping +3, +4, +3, +4 ...
--   from z = C2 = 36 (MIDI), so the map forms one continuous 20-note
--   alternating-thirds scale spanning 36..102.
keyRows :: [[(Char, Int)]]
keyRows =
    [ [('1', 88 - 12), ('2', 92 - 12), ('3', 95 - 12), ('4', 99 - 12), ('5', 102 - 12)]  -- number row
    , [('q', 71 - 12), ('w', 74 - 12), ('e', 78 - 12), ('r', 81 - 12), ('t', 85 - 12)]   -- qwert
    , [('a', 53 - 12), ('s', 57 - 12), ('d', 60 - 12), ('f', 64 - 12), ('g', 67 - 12)]   -- asdfg
    , [('z', 36 - 12), ('x', 39 - 12), ('c', 43 - 12), ('v', 46 - 12), ('b', 50 - 12)]   -- zxcvb
    ]
-- | Look up the MIDI note for a keyboard character (Nothing = not mapped).
charToNote :: Char -> Maybe Int
charToNote c = lookup c (concat keyRows)

-- | Key-down velocity for mapped keys.
defKeyVelocity :: Int
defKeyVelocity = 100

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (Char '=') Down _ _) w0 = startTuningSelection Next w0

handleEvent (EventKey (Char '-') Down _ _) w0 = startTuningSelection Prev w0

handleEvent (EventKey (Char '=') Up _ _) w0 = stopTuningSelection w0

handleEvent (EventKey (Char '-') Up _ _) w0 = stopTuningSelection w0

-- Preset switching: ',' previous / '.' next (scroll while held, like '='/'-'
-- for tunings).  The engine and audio callback pick the new voice from the
-- World on their next pass; the parameters are pushed by stepPreset.
handleEvent (EventKey (Char ',') Down _ _) w0 = startPresetSelection Prev w0

handleEvent (EventKey (Char '.') Down _ _) w0 = startPresetSelection Next w0

handleEvent (EventKey (Char ',') Up _ _) w0 = stopPresetSelection w0

handleEvent (EventKey (Char '.') Up _ _) w0 = stopPresetSelection w0

-- 'Keyboard' here refers to keyboard for typing
handleEvent (EventKey (Char '[') Down _ _) w0 = return w0 {
    keyboardShift = keyboardShift w0 - 1
    }

handleEvent (EventKey (Char ']') Down _ _) w0 = return w0 {
    keyboardShift = keyboardShift w0 + 1
    }

-- Backspace = panic button: release every held note at once.
--  Clearing midiEvtBuf makes the next cleanBuffer pass ship an empty frame,
--  so the engine's Phase 2 releases every sounding voice (physical keys
--  stay silent until released and re-pressed).
--  This project's GLUT/freeglut backend reports Backspace as Char '\8';
--  the GLFW backend reports SpecialKey KeyBackspace.  Handle both.
handleEvent (EventKey (SpecialKey KeyBackspace) Down _ _) w0 = clearMidiBuffer w0
handleEvent (EventKey (Char '\8') Down _ _) w0 = clearMidiBuffer w0

-- Computer keyboard as a MIDI keyboard: key-down sends NoteOn (vel 100),
-- key-up sends NoteOff.  appendMidiEvent drops the event into the shared
-- MidiEventBuffer; cleanBuffer re-ships held NoteOns every frame, so the
-- voice keeps sounding until the matching NoteOff arrives.
handleEvent (EventKey (Char c) Down _ _) w0
    | Just noteId <- charToNote c =
        appendMidiEvent (MidiEvent 1 NoteOn (noteId + keyboardShift w0) defKeyVelocity) w0
    | otherwise = return w0

handleEvent (EventKey (Char c) Up _ _) w0
    | Just noteId <- charToNote c =
        appendMidiEvent (MidiEvent 1 NoteOff (noteId + keyboardShift w0) 0) w0
    | otherwise = return w0
handleEvent _ w0 = return w0

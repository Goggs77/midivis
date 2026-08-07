{-# LANGUAGE BangPatterns #-}
-- | Keyboard control mapping.
--
--   Layer above 'CtrlBus': a logical control role (mod wheel, knobs,
--   sliders, shift button) is resolved to a MIDI CC number through the
--   active 'KeyboardLayout', then read back from the bus — raw or
--   normalised to [0,1] for direct modulation use.
--
--   Layouts are a polymorphic sum so new devices only add a constructor.
--   'Unknown' deliberately reuses the MiniLab3 mapping: code written against
--   a real keyboard keeps working until a specific device is configured.
module Midivis.Synth.KeyboardLayout
    ( KeyboardLayout(..)
    , CtrlRole(..)
    , roleToCC
    , readRole, readRoleNorm
    ) where

import Prelude
import Midivis.Synth.CtrlBus (CtrlBus, readCC, ccNorm)

-- | Known keyboard layouts.  Add a constructor per supported device;
--   'Unknown' is the placeholder that mirrors the MiniLab3 layout.
data KeyboardLayout
    = MiniLab3
    | Unknown
    deriving (Eq, Show)

-- | Logical controls a layout exposes to the synth.
data CtrlRole
    = ModWheel
    | Knob1 | Knob2 | Knob3 | Knob4 | Knob5 | Knob6 | Knob7 | Knob8
    | Slider1 | Slider2 | Slider3 | Slider4
    | ShiftButton
    deriving (Eq, Show)

-- | Resolve a control role to its MIDI CC number for the given layout.
--   MiniLab3 wiring: CC1 = mod wheel, CC86/87/89/90/110/111/116/117 = knobs
--   1-8, CC14/15/30/31 = sliders 1-4, CC27 = shift button.
roleToCC :: KeyboardLayout -> CtrlRole -> Int
roleToCC MiniLab3 r = roleToCC Unknown r   -- Unknown mirrors MiniLab3 for now
roleToCC Unknown r = case r of
    ModWheel   -> 1
    Knob1 -> 86
    Knob2 -> 87
    Knob3 -> 89
    Knob4 -> 90
    Knob5 -> 110
    Knob6 -> 111
    Knob7 -> 116
    Knob8 -> 117
    Slider1 -> 14
    Slider2 -> 15
    Slider3 -> 30
    Slider4 -> 31
    ShiftButton -> 27

-- | Raw 0-127 reading of a control role on the given layout.
readRole :: CtrlBus -> KeyboardLayout -> CtrlRole -> IO Int
readRole bus layout role = readCC bus (roleToCC layout role)

-- | Normalised [0,1] reading of a control role — the convenient form for
--   driving modulation amounts (LFO depth, reverb mix, ...).
readRoleNorm :: CtrlBus -> KeyboardLayout -> CtrlRole -> IO Double
readRoleNorm bus layout role = ccNorm bus (roleToCC layout role)

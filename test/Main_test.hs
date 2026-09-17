module Main (main) where

import Control.Concurrent.STM (newTQueueIO)
import qualified Data.Vector.Storable as V
import Graphics.Gloss.Relative

import Midivis.Tuning.Scala
import Midivis.World
import Midivis.EventHandler
import Midivis.System.MidiQuery (bufferMidi, initMidi)
import Midivis.Midi.MidiParser (MidiEvent(..))
import Midivis.Midi.MidiEventType (MidiEventType(NoteOn, NoteOff))

-- gloss-relative EventKey 是记录: EventKey { eventKey, eventKeyState, eventModifiers, eventMouse }
keyEv :: Key -> KeyState -> Event
keyEv k st = EventKey
    { eventKey = k
    , eventKeyState = st
    , eventModifiers = Modifiers Down Down Down
    , eventMouse = Mouse (0, 0) []
    }

main :: IO ()
main = do
    -- 原 Scala 测试
    let tuning = Scala "test" 1 "test tuning" 2 [2]
        calibrated = calibrate tuning 1 440
    if abs (getFreq calibrated 1 - 440) < 1e-9
        then pure ()
        else error "calibrate/getFreq should preserve the requested frequency"

    -- === 键盘→音符映射验证 ===
    -- 1. 结构: 4 行 × 5 键, 行序从上到下
    if length keyRows /= 4 || not (all (\row -> length row == 5) keyRows)
        then error "keyRows should be 4 rows of 5 keys"
        else pure ()

    -- 2. 步长: 从下到上、每行从左到右, +3/+4 交替, 基准键是 'z' (z 基准值
    --    不硬编码 —— 用户可整体移调, 测试只验证结构)
    let flat = concat (reverse keyRows)
        notes = map snd flat
        steps = zipWith (-) (drop 1 notes) notes
        expected = cycle [3, 4]
        zNote = snd (head flat)
    if fst (head flat) /= 'z'
        then error "bottom row should start with 'z'"
        else pure ()
    putStrLn $ "z note = " ++ show zNote
    if and (zipWith (==) steps (take (length steps) expected))
        then pure ()
        else error ("steps should be +3/+4 alternating, got " ++ show steps)

    -- 3. 查表自洽: 每个键都能查到, 且与 keyRows 一致; 无重复键
    let flatAll = concat keyRows
        dupKeys = [ c | (c, _) <- flatAll, length (filter ((== c) . fst) flatAll) > 1 ]
    if null dupKeys
        then pure ()
        else error ("duplicate keys in keyRows: " ++ show dupKeys)
    if and [charToNote c == Just n | (c, n) <- flatAll]
        then pure ()
        else error "charToNote should match keyRows for every key"

    -- 4. handleEvent 注入: Down 'z' → NoteOn zNote 100; Up 'z' → NoteOff zNote 0
    tq <- newTQueueIO
    let zNote = snd (head (concat (reverse keyRows)))
    w1 <- handleEvent (keyEv (Char 'z') Down) =<< initWorld tq
    let evts1 = V.toList (unpack (midiEvtBuf w1))
    if evts1 == [MidiEvent 1 NoteOn zNote 100]
        then pure ()
        else error ("Down 'z' should emit NoteOn " ++ show zNote ++ " 100, got " ++ show evts1)
    w2 <- handleEvent (keyEv (Char 'z') Up) w1
    let evts2 = V.toList (unpack (midiEvtBuf w2))
    if evts2 == [MidiEvent 1 NoteOn zNote 100, MidiEvent 1 NoteOff zNote 0]
        then pure ()
        else error ("Up 'z' should emit NoteOff " ++ show zNote ++ " 0, got " ++ show evts2)
    w3 <- handleEvent (keyEv (Char '=') Down) w2
    if tuningScroll w3 == Next
        then pure ()
        else error "Down '=' should scroll tuning"
    w4 <- handleEvent (keyEv (Char ' ') Down) w3
    if V.length (unpack (midiEvtBuf w4)) == 2
        then pure ()
        else error "unmapped key should not add events"

    -- 5. Backspace 清空所有 NoteOn（两条后端路径: GLUT Char '\8' / GLFW KeyBackspace）
    b1 <- handleEvent (keyEv (Char '\8') Down) w4
    if V.null (unpack (midiEvtBuf b1))
        then pure ()
        else error "Backspace (Char 8) should clear all held NoteOns"
    b2 <- handleEvent (keyEv (Char 'z') Down) b1
    if V.length (unpack (midiEvtBuf b2)) == 1
        then pure ()
        else error "re-press after backspace should work"
    b3 <- handleEvent (keyEv (SpecialKey KeyBackspace) Down) b2
    if V.null (unpack (midiEvtBuf b3))
        then pure ()
        else error "Backspace (KeyBackspace) should clear all held NoteOns"

    -- 6. Preset switching: ',' = Prev, '.' = Next (scroll), Up = Stop
    p1 <- handleEvent (keyEv (Char ',') Down) b3
    if presetScroll p1 == Prev
        then pure ()
        else error "Down ',' should scroll preset Prev"
    p2 <- handleEvent (keyEv (Char '.') Down) b3
    if presetScroll p2 == Next
        then pure ()
        else error "Down '.' should scroll preset Next"
    p3 <- handleEvent (keyEv (Char '.') Up) p2
    if presetScroll p3 == Stop
        then pure ()
        else error "Up '.' should stop preset scroll"

    -- === MIDI 设备缺失降级验证 ===
    -- 1. bufferMidi Nothing 是 no-op（无设备时不崩、不改 World）
    w5 <- bufferMidi Nothing w4
    if V.length (unpack (midiEvtBuf w5)) == 2
        then pure ()
        else error "bufferMidi Nothing should be a no-op"
    -- 2. initMidi 永不抛异常（有设备 → Just，无设备 → Nothing）
    dev <- initMidi
    case dev of
        Nothing -> putStrLn "no MIDI device present (computer keyboard only)"
        Just _  -> putStrLn "MIDI device present"

    putStrLn "keyboard map: all checks passed"

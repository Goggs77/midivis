module Main (main) where

import Control.Concurrent.STM (atomically, newTQueueIO, tryReadTQueue)
import qualified Data.Vector.Storable as V
import Graphics.Gloss.Relative

import Midivis.Tuning.Scala
import Midivis.World
import Midivis.EventHandler
import Midivis.System.MidiQuery (initMidi, midiPollThread)
import Midivis.System.ExampleRenderer (stepPreset, stepTuning)
import Midivis.Midi.MidiParser (MidiEvent(..))
import Midivis.Midi.MidiEventType (MidiEventType(NoteOn, NoteOff))
import Midivis.Synth.Preset (Voice(..), FxParams(..), EqBand(..), piano)

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
    -- Piano top-end EQ: a gentle low-pass above the audible fundamental range.
    case reverse (fxEqBands (vFx piano)) of
        EqBand freq gain shape q : _ ->
            if abs (freq - 18000.0) < 1e-9
                && abs gain < 1e-9
                && shape == 5/7
                && q <= 0.5
                then pure ()
                else error "piano final EQ band should be a soft 18 kHz low-pass"
        [] -> error "piano preset should define EQ bands"

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
    let tuningBefore = sclIndex w2
    w3 <- handleEvent (keyEv (Char '=') Down) w2
    if tuningScroll w3 == Next && sclIndex w3 /= tuningBefore
        then pure ()
        else error "Down '=' should change tuning immediately"
    earlyTuning <- stepTuning w3
    if sclIndex earlyTuning == sclIndex w3
        then pure ()
        else error "held tuning key repeated before its initial delay"
    dueTuning <- stepTuning w3 { time = tuningRepeatAt w3 }
    if sclIndex dueTuning /= sclIndex w3
        then pure ()
        else error "held tuning key should repeat when due"
    tuningUp <- handleEvent (keyEv (Char '=') Up) dueTuning
    stoppedTuning <- stepTuning tuningUp { time = 1.0e9 }
    if sclIndex stoppedTuning == sclIndex tuningUp
        then pure ()
        else error "tuning repeat continued after key-up"
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
    let presetBefore = presetIndex b3
    p1 <- handleEvent (keyEv (Char ',') Down) b3
    if presetScroll p1 == Prev && presetIndex p1 /= presetBefore
        then pure ()
        else error "Down ',' should change preset immediately"
    p2 <- handleEvent (keyEv (Char '.') Down) b3
    if presetScroll p2 == Next && presetIndex p2 /= presetBefore
        then pure ()
        else error "Down '.' should change preset immediately"
    earlyPreset <- stepPreset p2
    if presetIndex earlyPreset == presetIndex p2
        then pure ()
        else error "held preset key repeated before its initial delay"
    duePreset <- stepPreset p2 { time = presetRepeatAt p2 }
    if presetIndex duePreset /= presetIndex p2
        then pure ()
        else error "held preset key should repeat when due"
    p3 <- handleEvent (keyEv (Char '.') Up) p2
    if presetScroll p3 == Stop
        then pure ()
        else error "Up '.' should stop preset scroll"
    stoppedPreset <- stepPreset p3 { time = 1.0e9 }
    if presetIndex stoppedPreset == presetIndex p3
        then pure ()
        else error "preset repeat continued after key-up"

    -- === MIDI 设备缺失降级验证 ===
    -- 1. 无 MIDI 设备时轮询线程立即退出且不产生事件
    pollQueue <- newTQueueIO
    midiPollThread Nothing pollQueue
    noEvent <- atomically $ tryReadTQueue pollQueue
    case noEvent of
        Nothing -> pure ()
        Just _  -> error "midiPollThread Nothing should not enqueue events"
    -- 2. initMidi 永不抛异常（有设备 → Just，无设备 → Nothing）
    dev <- initMidi
    case dev of
        Nothing -> putStrLn "no MIDI device present (computer keyboard only)"
        Just _  -> putStrLn "MIDI device present"

    putStrLn "keyboard map: all checks passed"

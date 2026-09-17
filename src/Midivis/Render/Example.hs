{-# LANGUAGE BlockArguments #-}

module Midivis.Render.Example where

import Data.Maybe
import qualified Data.Vector.Storable as V
import Data.Vector.Storable ( toList )
import qualified Data.StorableVector as SV
import Graphics.Gloss.Relative
import Graphics.Gloss.Relative.Interface
import Data.Tuple.Extra (uncurry3)
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)
import qualified Data.Map.Strict as M

import Midivis.Tuning.Ascl 
import Midivis.Tuning.Poly
import Midivis.Tuning.Corpus
import Midivis.Midi.MidiParser
import Midivis.Util.Math
import Midivis.World
import Midivis.Synth.Types
import Midivis.Synth.VoicePool
import Midivis.Midi.MidiEventType (MidiEventType(..))
import Midivis.Render.Text (utf8Banner, TextSize(..), titleSize, subtitleSize, bodySize, TextFill(..), WrapMethod (WrapByWord, WrapByChar), LineWidthMode(..), utf8TextBox, TextDebug (..))
import Midivis.Synth.CtrlBus
import Midivis.Render.ShaderFX (shaderIdentity, shaderInvert, shaderGrayscale, shaderBlur, shaderTodo2, shaderPassUniform1fv)
import Midivis.Synth.KeyboardLayout

rgba :: Float -> Float -> Float -> Float -> Color
rgba = makeColor
rgbaI :: Int -> Int -> Int -> Int -> Color
rgbaI = makeColorI

unifiedWhite :: Color
unifiedWhite = rgba 0.99 0.98 0.98 0.95

-- w0 contains datas for rendering: sclOfChoice and baseAngle, the latter of which is updated on step
drawRelativeFrame :: World -> IO Frame
drawRelativeFrame w0 = do 
    if (not $ initialized w0) 
    then return $ Overlay [Aspect (0.9, 0.9) alignCenter $ solidEllipse (rgba 1 1 1 0.2),
        Zoom (Relative 0.2) (Relative 0.05) alignCenter $ 
            utf8TextBox bodySize WrapByWord FixedFrameWidth 12 FillNative alignCenter DebugNone 
                (show (length polyNames) ++ "/" ++ show (length allPoly)) unifiedWhite]
    else do
        -- Use the CURRENT window size (not the screen) so that the frame
        -- layout adapts to window resizes; otherwise Aspect ratios keep
        -- the startup screen aspect and the picture gets stretched when
        -- the user resizes the window.
        (w, h) <- fromIntegralPoint <$> getWindowSize
        -- Square side for the circular disc: the largest inscribed square
        -- of the window, scaled by 0.9.  Using min(w,h) (instead of just
        -- h) makes the disc follow BOTH window dimensions when resizing;
        -- the old (h/w*0.9, 0.9) made the side = 0.9h, so horizontal
        -- resizes left it unchanged.
        let m = Prelude.min w h
            mainColor = unifiedWhite -- <- (\ x -> withBlue (realToFrac (1 - 0.1*x)) unifiedWhite) <$> (ccNorm (ctrlBus w0) $ roleToCC MiniLab3 ModWheel)
        let centerFrame = Aspect (w, h) alignCenter .
                zoom (Relative (m/w*0.8)) (Relative (m/h*0.8)) alignCenter . debugAddBorder
            bottomFrame = Aspect (1, 0.73) alignBottom .
                zoom (Relative 1) (Relative 0.05) (alignAt (Relative 0, Relative (-1))) . debugAddBorder .
                zoom (Relative 1) (Relative 0.6666) alignCenter
            leftFrame = Aspect (1, 0.73) alignLeft . debugAddBorder .
                zoom (Relative (0.5 - m/w * 0.4041 )) (Relative (1.0)) alignLeft . debugAddBorder
            leftCenter = leftFrame . zoom (Relative 1) (Relative 0.333) alignRight . debugAddBorder
        notes <- mapToRadii w0 mainColor
        let centerPoint = centerFrame $
                solidEllipse (rgba 1 1 1 0.05)
            movingLine = centerFrame $
                notes
            middleCircle = centerFrame $
                thickEllipse (rgba 1 1 1 0.8) (Absolute 2.0)
                <> solidEllipse ((realToFrac (bufferTransparency w0 * 0.25 * tanh (ampsum w0))) `withAlpha` mainColor)
            leftText = leftCenter $
                utf8TextBox (TextSize 30) WrapByWord FixedFrameWidth 80 FillNative (alignRight) DebugNone (T.unpack $ chordName w0) mainColor
            leftLine = leftCenter $
                stroke 
                [(Relative 0.52, Relative (-(tanh $ 0.05+r)*2*tanh amp)), (Relative 0.52, Relative ((tanh $ 0.05+l)*2*tanh amp))]
                (rgba 1 1 1 (0.2 * amp ** 2))
            --leftColumn = column [leftText, leftText, leftText]
            bottomText = bottomFrame $
                utf8Banner bodySize FillFit alignCenter (T.unpack $ tunName $ sclOfChoice w0) mainColor
            -- Output RMS meter (smoothed L/R from the audio callback):
            -- two bars, bottom-right; width ~ 4 * RMS, clamped to the frame.
            rmsMeter =
                let bar col wdt yOff =
                        zoom (Relative wdt) (Relative 0.06) (alignAt (Relative 1, Relative yOff)) $
                            solid col
                in Overlay
                    [ bar (rgba 0.85 0.35 0.35 0.9) (l * 0.9) (-0.86)
                    , bar (rgba 0.35 0.75 0.45 0.9) (r * 0.9) (-0.79)
                    ]
        setTitleOnce $ synopsis $ sclOfChoice w0
        st <- stereoGraph w0
        let mainFrame = Overlay [movingLine, centerPoint, leftText, leftLine, middleCircle, centerFrame st, bottomText]
        -- Feed the TODO2 shader 129 float angles: one per polyphony slot
        -- (held-note angles from the same formula as mapToRadii, 0-padded) +
        -- baseAngle as the 129th element.  GLSL: uniform float uAngles[129];
        -- The TODO2 pass layers ON TOP of the invert pass (invert runs first
        -- on mainFrame, then todo2 post-processes the inverted result).
        let evts    = unpack $ midiEvtBuf w0
            noteOns = V.filter (\e -> evt e == NoteOn) evts
            freqs   = V.map (getFreq (sclOfChoice w0)) (V.map valueL noteOns)
            angles  = V.map (\x -> 2 * pi * log2 (x / freqA4 w0) + baseAngle w0) freqs
            nAng    = Prelude.min 128 (V.length angles)
            angList = toList $ V.map realToFrac (V.take nAng angles)
                      V.++ V.replicate (128 - nAng) 0.0
                      V.++ V.singleton (realToFrac (baseAngle w0))   -- 129 total
            shaderSetup s = shaderPassUniform1fv s "uAngles" angList
        return $ fxWith shaderTodo2 shaderSetup (fx shaderInvert mainFrame)
        where 
            amp = realToFrac $ ampsum w0
            l = realToFrac (Prelude.min 1 (rmsSmL w0 * 2)) :: Float
            r = realToFrac (Prelude.min 1 (rmsSmR w0 * 2)) :: Float


-- | Cycle the FX shader every 'fxFramePeriod' frames:
--   identity → grayscale → invert → blur, then back.
demoShader :: Int -> Shader
demoShader i =
    case i of
        0 -> shaderIdentity
        1 -> shaderGrayscale
        2 -> shaderInvert
        _ -> shaderBlur

-- | Set the window title only when it changed.  The title is a frame-stable
--   tuning description (synopsis); a per-frame system call is pure waste.
lastTitleRef :: IORef T.Text
lastTitleRef = unsafePerformIO (newIORef T.empty)
{-# NOINLINE lastTitleRef #-}

setTitleOnce :: T.Text -> IO ()
setTitleOnce t = do
    old <- readIORef lastTitleRef
    when (old /= t) $ do
        writeIORef lastTitleRef t
        _ <- setWindowTitle t
        return ()

-- | Radius mapper that uses midiEvtBuf AND cc
--   pl(radius) = radii
--   NOTE: deliberately NOT memoised.  The note ring continuously rotates
--   (stepAngle advances baseAngle by bufferVelocity, clockVelocity is a
--   nonzero constant), so any key containing baseAngle would miss every
--   frame — the cache cost (sort + Map insert) would exceed the rebuild
--   it saves.  See 2026-08-22 log.
mapToRadii :: World -> Color -> IO Frame
mapToRadii w0 clr = do
    mw <- ccNorm (ctrlBus w0) 1
    let evts   = unpack $ midiEvtBuf w0
        noteOns = V.filter (\e -> evt e == NoteOn) evts
        _names = V.map valueL noteOns
        freqs   = V.map (getFreq (sclOfChoice w0)) (V.map valueL noteOns)
        angles  = V.map (\x -> 2 * pi * log2 (x / freqA4 w0) + baseAngle w0) freqs
    amps <- V.mapM (\x -> readAmp (voicePool w0) (valueL x)) noteOns
    let lengths = V.map (scaleLog2 20 18000 0 1) freqs
        radii   = V.zipWith (\x y -> (x * 0.35 + 0.45 + 0.20 * mw) * y) amps lengths
    return $ Overlay $ zipWith3 (customRadius w0 mw clr) (toList angles) (toList radii) (toList _names)

toColor :: World -> Int -> VoiceParams -> Color
toColor _ id' vp =
  let h = scaleLog2 20 18000 0 (2 * pi) (vpFrequency vp)
      s = vpAmplitude vp
      v = fromIntegral id' + 1.0 / 128.0
   in hsv2rgb h s v

-- New algorithm dumped to drafts

-- | Creates a radius with mainColor, angle, radius
customRadius :: World -> Double -> Color -> Double -> Double -> Int -> Frame
customRadius w0 mw clr angle radius nameIndex  = 
    (zoom (Relative 0.25) (Relative 0.033) (alignAt (p2 .* 1.2)) . debugAddBorder) 
    (utf8Banner subtitleSize FillFit alignCenter (T.unpack $ getGeneralName (sclOfChoice w0) nameIndex) clr)
    <> aspect (1,1) alignCenter (zoom (Relative 0.20) (Relative 0.20) (alignAt (p2 .* 1.2)) (solidEllipse (0.008 `withAlpha` clr))) 
    <> stroke [zeroRelative, p2 .* 0.86] clr 
    -- <> fit (mainColor clr $ arc (realToFrac $ rad2deg angle) (realToFrac $ rad2deg $ angle + phaseDiff) (realToFrac $ 0.43*radius))
    where 
        p2 = (Relative $ realToFrac . (0.5*radius*) . cos $ angle + noise, Relative $ realToFrac.(0.5*radius*).sin $ angle - noise)
        noise = detNoise (d2i angle) (nameIndex) * 2 * mw ** 2

-- | Segment cache for the stereo waveform ring.  The ring buffer writes 2
--   samples per step at the pointer (stepRms), so a 'segSize'-sample
--   segment is rewritten at most once per (segSize/2) steps.  Each draw
--   rebuilds only the segment(s) the ring pointer passed since the last
--   draw and reuses every other segment, so the per-point polar/trig build
--   is amortised instead of being redone for all points every frame.
--   NOTE: the polyline is still rasterised in full every frame (the content
--   genuinely changes each draw — the step chain runs faster than the draw
--   loop), so the cache removes the *build* cost, not the raster cost.
segSize :: Int
segSize = 32

-- ring size at last draw (a change forces a full rebuild)
segLastSzRef :: IORef Int
segLastSzRef = unsafePerformIO (newIORef 0)
{-# NOINLINE segLastSzRef #-}

-- ring pointer at last draw
segLastIdxRef :: IORef Int
segLastIdxRef = unsafePerformIO (newIORef (-1))
{-# NOINLINE segLastIdxRef #-}

-- segment index -> frame
segCacheRef :: IORef (M.Map Int Frame)
segCacheRef = unsafePerformIO (newIORef M.empty)
{-# NOINLINE segCacheRef #-}

-- | Which segments the ring pointer has passed since the last draw.
--   @last@ = -1 means the first draw (everything is dirty).
dirtySegs :: Int -> Int -> Int -> [Int]
dirtySegs last cur nSeg
    | last < 0    = [0 .. nSeg - 1]
    | last == cur = []
    | last < cur  = [last `div` segSize .. cur `div` segSize]
    | otherwise   = [last `div` segSize .. nSeg - 1] ++ [0 .. cur `div` segSize]

stereoGraph :: World -> IO Frame
stereoGraph w0 = do
    let v     = sampleLRCache w0
        n     = V.length v
        idx   = sampleLRindex w0
        nSeg  = Prelude.max 1 ((n + segSize - 1) `div` segSize)
        rho a = abs $ 0.98 * tanh (2 * a)
        theta x = 2 * pi * fromIntegral x / fromIntegral n
        segFrame k =
            let lo = k * segSize
                hi = Prelude.min n (lo + segSize)
                pts = [ if odd x then polarPoint (rho (v `V.unsafeIndex` x)) (theta x + pi)
                                 else polarPoint (rho (v `V.unsafeIndex` x)) (theta x)
                      | x <- [lo .. hi - 1] ]
            in stroke pts (rgba 1 1 1 0.02)
    lastSz <- readIORef segLastSzRef
    last   <- readIORef segLastIdxRef
    cache  <- readIORef segCacheRef
    let cache'
          | lastSz /= n = M.fromList [ (k, segFrame k) | k <- [0 .. nSeg - 1] ]
          | otherwise   = foldl (\m k -> M.insert k (segFrame k) m) cache (dirtySegs last idx nSeg)
        frs = [ f | (_, f) <- M.toAscList cache' ]
    writeIORef segLastSzRef n
    writeIORef segLastIdxRef idx
    writeIORef segCacheRef cache'
    return $ Overlay frs
polarPoint :: Double -> Double -> UnitPoint
polarPoint rho theta = 
    let x = realToFrac $ rho * cos theta 
        y = realToFrac $ rho * sin theta
    in (Relative x, Relative y)

point :: Double -> Double -> UnitPoint
point x y = (Relative $ realToFrac x, Relative $ realToFrac y)

debugAddBorders :: [Frame] -> [Frame]
debugAddBorders = map debugAddBorder

debugAddBorder :: Frame -> Frame
debugAddBorder f = f
--debugAddBorder f = border (Absolute 1.1) (rgba 0 1 0 0.9) f

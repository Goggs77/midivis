{-# LANGUAGE BlockArguments #-}

module Midivis.Render.Example where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Vector.Storable ( toList )
import Graphics.Gloss.Relative
import qualified Data.Text as T
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_, when)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (sizeOf)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Midivis.Tuning.Ascl 
import Midivis.Tuning.Corpus
import Midivis.Midi.MidiParser
import Midivis.Util.Math
import Midivis.World
import Midivis.Synth.Types
import Midivis.Synth.VoicePool
import Midivis.Midi.MidiEventType (MidiEventType(..))
import Midivis.Render.Text (utf8Banner, TextSize(..), subtitleSize, bodySize, TextFill(..), WrapMethod (WrapByWord), LineWidthMode(..), utf8TextBox, TextDebug (..))
import Midivis.Synth.CtrlBus
import Midivis.Render.ShaderFX (shaderIdentity, shaderInvert, shaderGrayscale, shaderBlur)

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
        return $ fx shaderInvert mainFrame
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

-- | Segment size for partial stereo waveform updates.
segSize :: Int
segSize = 32

data StereoBuffer = StereoBuffer
    { stereoBufferObject :: !GL.BufferObject
    , stereoBufferFloats :: !Int
    }

-- | Cached [x,y] normalized vertices.  Only segments touched by the ring
--   pointer are recomputed; the complete storable buffer is retained between
--   draws, so no per-frame point lists or Picture trees are allocated.
stereoVerticesRef :: IORef (V.Vector Float)
stereoVerticesRef = unsafePerformIO (newIORef V.empty)
{-# NOINLINE stereoVerticesRef #-}

stereoBufferRef :: IORef (Maybe StereoBuffer)
stereoBufferRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE stereoBufferRef #-}

-- Ring size at last draw (a change forces a full rebuild).
segLastSzRef :: IORef Int
segLastSzRef = unsafePerformIO (newIORef 0)
{-# NOINLINE segLastSzRef #-}

-- Ring pointer at last draw.
segLastIdxRef :: IORef Int
segLastIdxRef = unsafePerformIO (newIORef (-1))
{-# NOINLINE segLastIdxRef #-}

-- | Which segments the ring pointer has passed since the last draw.
--   @last@ = -1 means the first draw (everything is dirty).
dirtySegs :: Int -> Int -> Int -> [Int]
dirtySegs prev cur nSeg
    | prev < 0    = [0 .. nSeg - 1]
    | prev == cur = []
    | prev < cur  = [prev `div` segSize .. cur `div` segSize]
    | otherwise   = [prev `div` segSize .. nSeg - 1] ++ [0 .. cur `div` segSize]

stereoPoint :: V.Vector Double -> Int -> Int -> (Float, Float)
stereoPoint samples n x =
    let a = samples `V.unsafeIndex` x
        rho = abs (0.98 * tanh (2 * a))
        theta = 2 * pi * fromIntegral x / fromIntegral n
              + if odd x then pi else 0
    in ( realToFrac (rho * cos theta)
       , realToFrac (rho * sin theta) )

stereoVertices :: V.Vector Double -> Int -> V.Vector Float
stereoVertices samples n =
    V.generate (2 * n) $ \j ->
        let (x, y) = stereoPoint samples n (j `div` 2)
        in if even j then x else y

updateStereoVertices :: V.Vector Double -> Int -> [Int] -> IO (V.Vector Float)
updateStereoVertices samples n dirtySegs' = do
    old <- readIORef stereoVerticesRef
    if V.length old /= 2 * n
        then pure (stereoVertices samples n)
        else do
            mutable <- V.unsafeThaw old
            forM_ dirtySegs' $ \k ->
                let lo = k * segSize
                    hi = Prelude.min n (lo + segSize)
                in forM_ [lo .. hi - 1] $ \x -> do
                    let (vx, vy) = stereoPoint samples n x
                    VM.write mutable (2 * x) vx
                    VM.write mutable (2 * x + 1) vy
            frozen <- V.unsafeFreeze mutable
            pure frozen

uploadStereoVertices :: GL.BufferObject -> V.Vector Float -> [Int] -> Int -> IO ()
uploadStereoVertices buffer vertices dirtySegs' n = do
    oldBuffer <- GL.get (GL.bindBuffer GL.ArrayBuffer)
    GL.bindBuffer GL.ArrayBuffer $= Just buffer
    let floatBytes = sizeOf (0 :: Float)
    V.unsafeWith vertices $ \base ->
        forM_ dirtySegs' $ \k -> do
            let lo = k * segSize
                hi = Prelude.min n (lo + segSize)
                byteOffset = lo * 2 * floatBytes
                byteCount = (hi - lo) * 2 * floatBytes
            GL.bufferSubData GL.ArrayBuffer GL.WriteToBuffer
                (fromIntegral byteOffset)
                (fromIntegral byteCount)
                (castPtr (base `plusPtr` byteOffset))
    GL.bindBuffer GL.ArrayBuffer $= oldBuffer

ensureStereoBuffer :: V.Vector Float -> Bool -> [Int] -> Int -> IO GL.BufferObject
ensureStereoBuffer vertices rebuild dirtySegs' n = do
    current <- readIORef stereoBufferRef
    let requiredFloats = 2 * n
    case current of
        Just target
            | stereoBufferFloats target == requiredFloats -> do
                when rebuild $
                    uploadStereoVertices
                        (stereoBufferObject target) vertices
                        (dirtySegs (-1) 0 (Prelude.max 1 ((n + segSize - 1) `div` segSize)))
                        n
                when (not rebuild && not (null dirtySegs')) $
                    uploadStereoVertices
                        (stereoBufferObject target) vertices dirtySegs' n
                pure (stereoBufferObject target)
        _ -> do
            forM_ current $ \target -> GL.deleteObjectName (stereoBufferObject target)
            buffer <- GL.genObjectName
            oldBuffer <- GL.get (GL.bindBuffer GL.ArrayBuffer)
            GL.bindBuffer GL.ArrayBuffer $= Just buffer
            V.unsafeWith vertices $ \base ->
                GL.bufferData GL.ArrayBuffer $=
                    ( fromIntegral (requiredFloats * sizeOf (0 :: Float))
                    , castPtr base
                    , GL.DynamicDraw )
            GL.bindBuffer GL.ArrayBuffer $= oldBuffer
            writeIORef stereoBufferRef $
                Just (StereoBuffer buffer requiredFloats)
            pure buffer

stereoGraph :: World -> IO Frame
stereoGraph w0 = do
    let v     = sampleLRCache w0
        n     = V.length v
        idx   = sampleLRindex w0
        nSeg  = Prelude.max 1 ((n + segSize - 1) `div` segSize)
    lastSz <- readIORef segLastSzRef
    prev   <- readIORef segLastIdxRef
    let rebuild = lastSz /= n || prev < 0
        dirty = if rebuild then [0 .. nSeg - 1] else dirtySegs prev idx nSeg
    vertices <- if rebuild
        then pure (stereoVertices v n)
        else updateStereoVertices v n dirty
    writeIORef stereoVerticesRef vertices
    buffer <- ensureStereoBuffer vertices rebuild dirty n
    strip <- lineStripBufferPicture buffer n
    let picture = Color (rgba 1 1 1 0.02) strip
    writeIORef segLastSzRef n
    writeIORef segLastIdxRef idx
    return $ Sized $ \(w,h) ->
        -- Match gloss-relative's Unit semantics: Relative 1.0 spans the
        -- full reference dimension, not half of it.
        Stretch (Just (w,h)) $ Scale (realToFrac w) (realToFrac h) picture

debugAddBorders :: [Frame] -> [Frame]
debugAddBorders = map debugAddBorder

debugAddBorder :: Frame -> Frame
debugAddBorder f = f
--debugAddBorder f = border (Absolute 1.1) (rgba 0 1 0 0.9) f

module Midivis.Render.Example where
import qualified Data.Vector.Storable as V
import Data.Vector.Storable ( toList )
import qualified Data.StorableVector as SV
import Graphics.Gloss.Relative
import Data.Tuple.Extra (uncurry3)
import qualified Data.Text as T

import Midivis.Tuning.Ascl 
import Midivis.Midi.MidiParser
import Midivis.Util.Math
import Midivis.World
import Midivis.Synth.Types
import Midivis.Synth.VoicePool
import Midivis.Midi.MidiEventType (MidiEventType(..))
import Midivis.Render.Text (utf8Banner, TextSize(..), titleSize, subtitleSize, bodySize, TextFill(..))
import Midivis.Synth.CtrlBus

rgba :: Float -> Float -> Float -> Float -> Color
rgba = makeColor
rgbaI :: Int -> Int -> Int -> Int -> Color
rgbaI = makeColorI


-- w0 contains datas for rendering: sclOfChoice and baseAngle, the latter of which is updated on step
drawRelativeFrame :: World -> IO Frame
drawRelativeFrame w0 = do 
    let centerFrame = Aspect (1,1) alignCenter .
            zoom (Relative 0.8) (Relative 0.8) alignCenter . debugAddBorder
        bottomFrame = Aspect (1, 0.73) alignBottom . debugAddBorder .
            zoom (Relative 0.8) (Relative 0.104) (alignAt (Relative 0, Relative (-1))) . debugAddBorder .
            zoom (Relative 1) (Relative 0.333) alignCenter
    notes <- mapToRadii w0 (rgba 1 1 1 0.95)
    let centerPoint = centerFrame $
            solidEllipse (rgba 1 1 1 0.05)
        movingLine = centerFrame $
            notes
        middleCircle = centerFrame $
            thickEllipse (rgba 1 1 1 0.8) (Absolute 2.0)
            <> solidEllipse ((realToFrac $ bufferTransparency w0) `withAlpha` white)
        bottomText = bottomFrame $
            utf8Banner bodySize FillFit alignCenter (T.unpack $ tunName $ sclOfChoice w0) (rgba 1 1 1 0.95)
    return $ Overlay [movingLine, centerPoint, middleCircle, bottomText]

-- pl(radius) = radii
mapToRadii :: World -> Color -> IO Frame
mapToRadii w0 clr = do
    let evts = unpack $ midiEvtBuf w0
        noteOns = V.filter (\e -> evt e == NoteOn) evts
        freqs = toList $ V.map (getFreq (sclOfChoice w0)) (V.map valueL noteOns)
        names = map
            (T.unpack . getGeneralName (sclOfChoice w0) . valueL)
            (toList (V.filter (\ e -> evt e == NoteOn) evts))
        -- phaseDiff = 2*pi/fromIntegral (length freqs)
        aPh = map (\ x -> 2 * pi * log2 (x / freqA4 w0) + baseAngle w0) freqs
    amps <- V.toList <$> V.mapM (\ x -> readAmp (voicePool w0) (valueL x)) noteOns
    mw <- ccNorm (ctrlBus w0) 1
    let lengths = map (scaleLog2 20 18000 0 1) freqs
        radii = zipWith (\ x y -> (x * 0.35 + 0.45 + 0.20 * mw) * y) amps lengths
        -- zip for processing
    let zipped = zip3 aPh radii names 
    return $ Overlay $ map (uncurry3 (customRadius clr)) zipped


-- | Creates a radius with color, angle, radius
customRadius :: Color -> Double -> Double -> String -> Frame
customRadius clr angle radius na  = 
    (zoom (Relative 0.25) (Relative 0.025) (alignAt (p2 .* 1.2)) . debugAddBorder) (utf8Banner subtitleSize FillFit alignCenter na clr) 
    <> aspect (1,1) alignCenter (zoom (Relative 0.20) (Relative 0.20) (alignAt (p2 .* 1.2)) (solidEllipse (0.033 `withAlpha` clr))) 
    <> stroke [zeroRelative, p2 .* 0.86] clr 
    -- <> fit (color clr $ arc (realToFrac $ rad2deg angle) (realToFrac $ rad2deg $ angle + phaseDiff) (realToFrac $ 0.43*radius))
    where p2 = (Relative $ realToFrac.(0.5*radius*).cos $ angle, Relative $ realToFrac.(0.5*radius*).sin $ angle)
    

debugAddBorders :: [Frame] -> [Frame]
debugAddBorders = map debugAddBorder

debugAddBorder :: Frame -> Frame
--debugAddBorder f = border (Absolute 2) (rgba 0 1 0 0.9) f -- delete comments for debug
debugAddBorder = id
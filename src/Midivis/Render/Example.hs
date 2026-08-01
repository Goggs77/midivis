module Midivis.Render.Example where
import qualified Data.Vector.Storable as V
import Data.Vector.Storable ( toList )
import Graphics.Gloss.Relative
import Data.Tuple.Extra (uncurry3)

import Midivis.Tuning.Scala
import Midivis.Midi.MidiParser
import Midivis.Util.Math
import Midivis.World
import Midivis.Midi.MidiEventType (MidiEventType(..))


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
    let centerPoint = centerFrame $
            solidEllipse (rgba 1 1 1 0.05)
        movingLine = centerFrame $
            mapToRadii w0 (rgba 1 1 1 0.95)
        middleCircle = centerFrame $
            thickEllipse (rgba 1 1 1 0.8) (Absolute 2.0)
        bottomText = bottomFrame $
            banner (name $ sclOfChoice w0) (rgba 1 1 1 0.95)
    return $ Overlay [movingLine, centerPoint, middleCircle, bottomText]

-- pl(radius) = radii
mapToRadii :: World -> Color -> Frame
mapToRadii w0 clr = 
    let evts = unpack $ midiEvtBuf w0
        freqs = toList $ V.map (getFreq (sclOfChoice w0)) (V.map valueL (V.filter (\e -> evt e == NoteOn) evts))
        names = map
            (getGeneralName (sclOfChoice w0) . valueL)
            (toList (V.filter (\ e -> evt e == NoteOn) evts))
        phaseDiff = 2*pi/fromIntegral (length freqs)
        aPh = zip (take (length freqs) (iterate (+phaseDiff) (baseAngle w0))) (replicate (length freqs) phaseDiff)
        -- zip for processing
        zipped = zip3 aPh (scaleNumLog2 20 18000 0 1 freqs) names 
    in Overlay $ map (uncurry3 (customRadius clr)) zipped


-- | Creates a radius with color, angle, radius
customRadius :: Color -> (Double, Double) -> Double -> String -> Frame
customRadius clr (angle, phaseDiff) radius na  =
    zoom (Relative 0.05) (Relative 0.05) (alignAt (p2 .* 1.2)) (banner na clr) 
    <> aspect (1,1) alignCenter (zoom (Relative 1) (Relative 1) (alignAt (p2 .* 1.2)) (solidEllipse (0.05 `withAlpha` clr))) 
    <> stroke [zeroRelative, p2 .* 0.86] clr 
    <> fit (color clr $ arc (realToFrac $ rad2deg angle) (realToFrac $ rad2deg $ angle + phaseDiff) (realToFrac $ 0.43*radius))
    where p2 = (Relative $ realToFrac.(0.5*radius*).cos $ angle, Relative $ realToFrac.(0.5*radius*).sin $ angle)
    

debugAddBorders :: [Frame] -> [Frame]
debugAddBorders = map (border (Absolute 2) (rgba 0 1 0 0.9))

debugAddBorder :: Frame -> Frame
--debugAddBorder f = border (Absolute 2) (rgba 0 1 0 0.9) f -- delete comments for debug
debugAddBorder = id
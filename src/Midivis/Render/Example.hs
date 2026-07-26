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

drawExampleText :: Picture
drawExampleText =  (scale 0.1 0.1 . color white) (Text "This is an example window")

-- w0 contains datas for rendering: sclOfChoice and baseAngle, the latter of which is updated on step
drawRelativeFrame :: World -> IO Frame
drawRelativeFrame w0 = do 
    let centerFrame = Aspect (1,1) alignCenter .
            zoom (Relative 0.73) (Relative 0.73) alignCenter
    let centerPoint = centerFrame $
            solidEllipse (rgba 1 1 1 0.05)
        movingLine = centerFrame $
            mapToRadii w0 (rgba 1 1 1 0.95)
        middleCircle = centerFrame $
            thickEllipse (rgba 1 1 1 0.8) (Absolute 2.0)
    return $ Overlay [movingLine, centerPoint, middleCircle]

-- pl(radius) = radii
mapToRadii :: World -> Color -> Frame
mapToRadii w0 clr = 
    let evts = unpack $ midiEvtBuf w0
        freqs = toList $ V.map (getFreq (sclOfChoice w0)) (V.map (\e -> valueL e) (V.filter (\e -> evt e == NoteOn) evts))
        names = map (getGeneralName (sclOfChoice w0)) (map (\e -> valueL e) (toList (V.filter (\e -> evt e == NoteOn) evts)))
        phaseDiff = 2*pi/fromIntegral (length freqs)
        angles = take (length freqs) (iterate (+phaseDiff) (baseAngle w0))
        -- zip for processing
        zipped = zip3 angles (scaleNumLog (1) (20000) 0.2 1 freqs) names 
    in Overlay $ map (uncurry3 (customRadius clr)) zipped


-- | Creates a radius with color, angle, radius
customRadius :: Color -> Double -> Double -> String -> Frame
customRadius clr angle radius na  =
    zoom (Relative 0.05) (Relative 0.05) (alignAt p2) (banner na clr) <>
    aspect (1,1) alignCenter (zoom (Relative 0.1) (Relative 0.1) (alignAt p2) (solidEllipse (0.2 `withAlpha` clr))) <>
    stroke [
        (Relative 0, Relative 0), 
        p2 .* 0.86
    ] clr
    where p2 = (Relative $ realToFrac.(0.5*radius*).cos $ angle, Relative $ realToFrac.(0.5*radius*).sin $ angle)
    

debugAddBorders :: [Frame] -> [Frame]
debugAddBorders [] = []
debugAddBorders (f:fs) = border (Absolute 2) (rgba 0 1 0 0.9) f : debugAddBorders fs

debugAddBorder :: Frame -> Frame
debugAddBorder f = border (Absolute 2) (rgba 0 1 0 0.9) f
module Midivis.Synth.Smooth where
import Midivis.Util.Math
data Smooth = Smooth {value :: Double, destination, lerpFactor :: Double}

stepSmooth :: Smooth -> Smooth
stepSmooth s = s{value = lerp (value s) (destination s) (lerpFactor s)}


module Midivis.Synth.Osc where

import qualified Data.StorableVector.Base as SV
import qualified Synthesizer.Storable.Oscillator as O
import qualified Synthesizer.Basic.Phase as P
import qualified Synthesizer.Basic.Wave as W
import qualified Synthesizer.Storable.Signal as S
import Midivis.Synth.Types 


myWave :: W.T Double Double
myWave = W.fastSine4LeastSquares

initialPhase :: P.T Double
initialPhase = P.fromRepresentative 0

audioSignal :: S.T Double -> S.T Double
audioSignal freqSig = O.freqMod defChunkSize myWave initialPhase freqSig

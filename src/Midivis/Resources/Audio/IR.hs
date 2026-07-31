{-# LANGUAGE TemplateHaskell #-}
module Midivis.Resources.Audio.IR where
import Data.FileEmbed
import Midivis.Synth.WavLoader (loadWavMono)

ir = loadWavMono $(embedFileRelative "assets/audio/IR/ir.wav")
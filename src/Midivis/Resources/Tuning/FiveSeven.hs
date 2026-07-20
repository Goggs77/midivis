{-# LANGUAGE TemplateHaskell #-}
module Midivis.Resources.Tuning.FiveSeven where
import qualified Data.ByteString as BS
import Data.FileEmbed

scala = $(embedFileRelative "assets/tuning/7-EDO + 5-EDO.scl")

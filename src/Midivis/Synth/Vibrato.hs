{-# LANGUAGE BangPatterns #-}
-- | Pitch vibrato: a pure per-sample frequency ratio from a sine LFO.
--   Applied at the voice's phase increment (step = freq/sr × ratio), so it
--   is a true pitch vibrato — distinct from the amplitude LFOs inside
--   'vSample'.  Depth is modulatable; rate comes from the preset.
module Midivis.Synth.Vibrato
    ( vibRatio
    ) where

import Prelude

-- | Frequency ratio at absolute sample time @t@: 1 + depth·sin(2π·rate·t/sr).
--   @depth@ is the peak ratio deviation (e.g. 0.005 ≈ ±5 cents); 0 = off.
--   Pure and allocation-free, safe for the per-sample real-time path.
vibRatio :: Double -> Double -> Double -> Double -> Double
vibRatio depth rate sr t =
    1.0 + depth * sin (2 * pi * rate * t / sr)

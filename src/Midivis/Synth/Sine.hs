{-# LANGUAGE BangPatterns #-}
-- | Nyquist-safe sine oscillator.
--
--   An oscillator whose frequency reaches or exceeds the Nyquist frequency
--   (defSampleRate / 2) can no longer be sampled faithfully — feeding it
--   straight through would alias below Nyquist and add audible garbage, so
--   it is forced to 0 instead.  This is the single choke point for every
--   partial in the preset sample functions.
--
--   Signature: frequency (Hz) -> phase in [0,1) -> sample value.
--
--   Phase semantics: the phase accumulator is advanced OUTSIDE this module
--   (Render advances it per sample by step = freq/sr, keeping ph in [0,1)
--   cycles), so the oscillator is simply sin(2π·phase).  @freq@ is used
--   ONLY as the Nyquist guard — the pitch lives in the external step, not
--   here.  (A DDS form sin(2π·freq·phase/sr) would require the caller to
--   pass a sample counter instead of a cycle fraction; the presets pass a
--   cycle fraction, so the two must not be mixed.)
--
--   The core is plain Prelude 'sin' for now (a band-limited table / polyBLEP
--   can replace it here without touching the presets).
module Midivis.Synth.Sine where

import Prelude
import Midivis.Synth.Types (defSampleRate)
import Data.Fixed (mod')

-- | Credit: https://mooooo.ooo/chebyshev-sine-approximation/
--   This is a fast sine implementation that guarantees error \lt 5 ULP
--   Generates more harmonic compared to Prelude.sin
{-# INLINE fastSine' #-}
fastSine' :: Double -> Double
fastSine' x' =
    let pi' = (3.1415927410125732 - 0.00000008742277657347586)
        x = x' `mod'` (2 * pi')
        x2 = x * x
        p11 = 1.32913446369766718120324917415992976452154154051525892e-10
        p9  = p11 * x2 - 2.33177897192836082466066115718536782354224647348350113e-8
        p7  = p9  * x2 + 2.52229235749396866288379170129828403876289663605034418e-6
        p5  = p7  * x2 - 0.000173505057912483501491115906801116298084629719204655552
        p3  = p5  * x2 + 0.00662087952180793343258682906697112938547424931632185616
        p1  = p3  * x2 - 0.101321183346709072589001712988183609230944236760490476
    in (x - pi') * (x + pi') * p1 * x



{-# INLINE triangle' #-}
triangle' :: Double -> Double
triangle' x' = 
    let x = x' - fromIntegral (floor x')
    in case x of
        n | 0 <= n && n < 0.25 -> 4 * x
        n | 0.25 <= n && n < 0.75 -> 2 - 4 * x
        n | 0.75 <= n && n <= 1.0 -> 4 * x - 4
        _ -> 0

{-# INLINE saw' #-}
saw' :: Double -> Double
saw' x = 
    let saws x' = 2 * x' - 2 * fromIntegral (floor x')
    in saws (x + 0.5) - 0.5

{-# INLINE satSaw' #-}
satSaw' :: Double -> Double -> Double
satSaw' sharp = tanh . (*sharp) . tan . (*pi)

-- | Nyquist frequency = half the project sample rate.
nyquist :: Double
nyquist = defSampleRate / 2

-- | sin(2π·phase) with the Nyquist guard: @freq >= defSampleRate/2@ → 0.
--   Phase is expected in [0,1) cycles; a wrapped phase outside the range
--   still works (sin is periodic).
--   Please note that freq is purely nyquist guard and 
--   we use the same multiplyer on phase for harmonics
{-# INLINE sine #-}
sine :: Double -> Double -> Double
sine freq phase
    | freq >= nyquist = 0.0
    | otherwise       = sin (2 * pi * phase)


-- | fastSine using chebyshev approximation with range wrapping to (-π, π)
--   this generates richer harmonics and speeds up calculations
{-# INLINE fastSine #-}
fastSine :: Double -> Double -> Double
fastSine freq phase
    | freq >= nyquist = 0.0
    | otherwise       = fastSine' (2 * pi * phase)

{-# INLINE triangle #-}
triangle :: Double -> Double -> Double
triangle freq phase
    | freq >= nyquist = 0.0
    | otherwise       = triangle' phase

{-# INLINE saw #-}
saw :: Double -> Double -> Double
saw freq phase
    | freq >= nyquist = 0.0
    | otherwise       = saw' phase

{-# INLINE satSaw #-}
satSaw :: Double -> Double -> Double
satSaw freq phase
    | freq >= nyquist = 0.0
    | otherwise       = satSaw' 1 phase

{-# INLINE satMod #-}
satMod :: Double -> Double -> Double -> Double
satMod sharp freq phase
    | freq >= nyquist = 0.0
    | otherwise       = satSaw' sharp phase

{-# INLINE sawPush #-}
sawPush :: Double -> Double -> Double -> Double
sawPush push freq phase
    | freq >= nyquist = 0.0
    | otherwise       = saw' (phase ** push)

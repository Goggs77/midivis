{-# LANGUAGE BangPatterns #-}
-- Verify the note-on snap fix on the user's piano config:
--   stage1 = lowpass, base cutoff 15000, mod -15000*env2 (target floor 20Hz)
-- Before fix: first audible block started from smoothed (cutoff=0 -> clamp
--   20Hz, shape=0 -> lowShelf), then swept up -> near-full mute -> click.
-- After fix: fresh snap -> coefficients already at target.
-- We compare the 8kHz sine gain of the three states.
import Midivis.Synth.ModMatrix (shapeOfFloat, FilterShape(..))

filterCoeffs :: Double -> Double -> Double -> Double -> Double
             -> (Double, Double, Double, Double, Double)
filterCoeffs sr g f0 q sh =
    case shapeOfFloat sh of
        FLowShelf -> lowShelf
        FPeak     -> peak
        FHighShelf -> highShelf
        FNotch    -> notch
        FBandpass -> bandpass
        FLowpass  -> lowpass
        FHighpass -> highpass
  where
    f0' = clamp (20.0, 0.98 * 0.5 * sr) f0
    g'  = clamp (-24.0, 24.0) g
    q'  = clamp (0.1, 10.0) q
    lowShelf =
        let a' = 10 ** (g' / 40.0)
            b  = sqrt (2.0 * a')
            bb = 2.0 * sqrt a' * b
            w' = 2.0 * pi * f0' / sr
            c' = cos w'; s' = sin w'
            a0 = (a' + 1) + (a' - 1) * c' + bb * s'
        in ( a' * ((a' + 1) - (a' - 1) * c' + bb * s') / a0
           , 2.0 * a' * ((a' - 1) - (a' + 1) * c') / a0
           , a' * ((a' + 1) - (a' - 1) * c' - bb * s') / a0
           , -2.0 * ((a' - 1) + (a' + 1) * c') / a0
           , ((a' + 1) + (a' - 1) * c' - bb * s') / a0 )
    highShelf =
        let a' = 10 ** (g' / 40.0)
            b  = 1.0 / sqrt (2.0 * a')
            bb = 2.0 * sqrt a' * b
            w' = 2.0 * pi * f0' / sr
            c' = cos w'; s' = sin w'
            a0 = (a' + 1) + (a' - 1) * c' + bb * s'
        in ( a' * ((a' + 1) - (a' - 1) * c' + bb * s') / a0
           , -2.0 * a' * ((a' - 1) + (a' + 1) * c') / a0
           , a' * ((a' + 1) - (a' - 1) * c' - bb * s') / a0
           , -2.0 * ((a' - 1) + (a' + 1) * c') / a0
           , ((a' + 1) + (a' - 1) * c' - bb * s') / a0 )
    peak =
        let a' = 10 ** (g' / 40.0)
            w' = 2.0 * pi * f0' / sr
            c' = cos w'; s' = sin w'
            al = s' / (2.0 * q')
            a0 = 1.0 + al / a'
        in ( (1.0 + al * a') / a0, -2.0 * c' / a0, (1.0 - al * a') / a0
           , -2.0 * c' / a0, (1.0 - al / a') / a0 )
    notch =
        let w' = 2.0 * pi * f0' / sr
            c' = cos w'; s' = sin w'
            al = s' / (2.0 * q')
            a0 = 1.0 + al
            n0 = 1.0 / a0
        in ( n0, -2.0 * c' / a0, n0, -2.0 * c' / a0, (1.0 - al) / a0 )
    bandpass =
        let w' = 2.0 * pi * f0' / sr
            c' = cos w'; s' = sin w'
            al = s' / (2.0 * q')
            a0 = 1.0 + al
            n0 = al / a0
        in ( n0, 0.0, -n0, -2.0 * c' / a0, (1.0 - al) / a0 )
    lowpass =
        let w' = 2.0 * pi * f0' / sr
            c' = cos w'; s' = sin w'
            al = s' / (2.0 * q')
            a0 = 1.0 + al
            b0 = (1.0 - c') / (2.0 * a0)
        in ( b0, 2.0 * b0, b0, -2.0 * c' / a0, (1.0 - al) / a0 )
    highpass =
        let w' = 2.0 * pi * f0' / sr
            c' = cos w'; s' = sin w'
            al = s' / (2.0 * q')
            a0 = 1.0 + al
            b0 = (1.0 + c') / (2.0 * a0)
        in ( b0, -2.0 * b0, b0, -2.0 * c' / a0, (1.0 - al) / a0 )

clamp :: Ord a => (a, a) -> a -> a
clamp (lo, hi) v = min hi (max lo v)

biquadStep :: (Double, Double, Double, Double, Double)
           -> (Double, Double, Double, Double)
           -> Double
           -> (Double, (Double, Double, Double, Double))
biquadStep (b0, b1, b2, a1, a2) (x1, x2, y1, y2) x =
    let y = b0 * x + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
    in (y, (x, x1, y, y1))

sineGain :: (Double, Double, Double, Double, Double) -> Double -> Int -> Double
sineGain c freq n =
    let sr = 48000.0
        w  = 2 * pi * freq / sr
        warm = 4096
        go :: Int -> (Double, Double, Double, Double) -> Double -> Double -> Double
        go i st !si !so
            | i >= n = sqrt (so / fromIntegral (n - warm))
            | otherwise =
                let y  = sin (w * fromIntegral i)
                    (yf, st') = biquadStep c st y
                    si' = if i >= warm then si + y * y else si
                    so' = if i >= warm then so + yf * yf else so
                in go (i + 1) st' si' so'
    in go 0 (0, 0, 0, 0) 0 0

main :: IO ()
main = do
    let sr = 48000.0
        -- user piano: stage1 = lowpass, base cutoff 15000, Q=1
        -- env2 = 1.0 (sustain) -> target = 15000 - 15000 = 0 -> clamp 20Hz
        -- BEFORE FIX: first block smoothed state (cutoff=0, shape=0) clamped
        beforeC = filterCoeffs sr 0 0 1.0 0.0        -- smoothed 0: cutoff->20Hz, shape->lowShelf
        -- AFTER FIX: fresh snap to clamped target
        afterC  = filterCoeffs sr 0 0 1.0 (5/7)      -- cutoff 20Hz but shape lowpass
        afterFull = filterCoeffs sr 0 15000 1.0 (5/7) -- true steady target if env2=0
        -- also the sweep midpoints the old code produced: cutoff 4500 (0.3*15000)
        midC    = filterCoeffs sr 0 4500 1.0 (5/7)
        g8k f = sineGain f 8000 8192
    putStrLn $ "8kHz gain, OLD first block (shape=lowShelf, cutoff~20): " ++ show (g8k beforeC)
    putStrLn $ "8kHz gain, snap (lowpass@20Hz):                        " ++ show (g8k afterC)
    putStrLn $ "8kHz gain, sweep midpoint (lowpass@4500):              " ++ show (g8k midC)
    putStrLn $ "8kHz gain, steady target (lowpass@15000):              " ++ show (g8k afterFull)
    putStrLn $ "click source (old shape sweep): " ++ show (abs (g8k beforeC - g8k midC))
    putStrLn $ "snap continuity (lowpass@20 -> 15000 gap): " ++ show (abs (g8k afterC - g8k afterFull))

{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
{-# LANGUAGE BangPatterns #-}
module Midivis.Synth.Render
    ( SynthFx(..)
    , renderCallback
    , renderVoices
    , pushVoiceToFx
    , newVFilterState
    , newVFilterFresh
    , widenR, dcBlockBuffer, compressBuffer
    ) where

import Prelude
import Control.Monad (forM, forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.IORef (IORef, readIORef, writeIORef)
import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, pokeByteOff, peekElemOff, pokeElemOff)
import Foreign.ForeignPtr (withForeignPtr)
import Data.Ord (clamp)

import Midivis.Synth.VoicePool
import Midivis.Synth.Types
import Midivis.Synth.ConvReverb (ConvReverb, processReverb, setConvMix, getConvMix)
import Midivis.Synth.AlgorithmicReverb (AlgReverb, AlgParams(..), processAlgReverb, setAlgParams, getAlgParams)
import Midivis.Synth.Eq (EqFx, processEq, setEqTargets)
import Midivis.Synth.Chorus (ChorusFx, processChorus, setChorusTargets, chorusBypass)
import Midivis.Synth.Ensemble (EnsembleFx, processEnsemble, setEnsembleTargets, ensembleBypass)
import Midivis.Synth.Vibrato (vibRatio)
import Midivis.Synth.ModMatrix (ModRow(..), ModSource(..), ModTarget(..), EqParam(..), FilterParam(..), FilterShape(..), shapeOfFloat, isPerVoice)
import Midivis.Synth.Preset (Voice(..), LfoParams(..), EnvParams(..), FxParams(..), ReverbParams(..), SynthCtx(..), EqBand(..), FilterParams(..), defFilterParams, noteLenAt, smoothNoise)
import Midivis.Synth.CtrlBus (CtrlBus, ccNorm)
import Midivis.Util.Math
import Data.Bits (shiftR, (.&.), xor)
import Data.Word (Word64)

-- DetNoise moved to Math.hs

-- | Deterministic white noise in [-0.01, 0.01]: a SplitMix64-style mix of
--   the sample position and slot.  'detNoise' folds the position through a
--   single multiply chain whose low-order structure survives into the
--   output bits, so successive samples stay correlated and the power
--   spectrum is coloured (not flat).  Here the xorshift-multiply cascade
--   decorrelates neighbours, giving a statistically flat spectrum (true
--   white noise) while staying pure and allocation-free.
detNoiseW :: Int -> Int -> Double
detNoiseW s slot =
    let z0 = fromIntegral s * 0x9E3779B97F4A7C15
           + fromIntegral (slot + 1) * 0xBF58476D1CE4E5B9 :: Word64
        z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
        z3 = z2 `xor` (z2 `shiftR` 31)
        u = fromIntegral (z3 `shiftR` 40) :: Double
    in (u / 8388607.5 - 1.0) * 0.01

--------------------------------------------------------------------------------
-- Per-voice filters (2 biquad stages per voice, before the mix)
--------------------------------------------------------------------------------

-- | RBJ biquad coefficients for one filter stage.  Shape from 'shapeOfFloat';
--   params clamped like the EQ core: cutoff [20, 0.98·Nyquist], gain [-24,24],
--   Q [0.1,10].  Returns (b0, b1, b2, a1, a2) for transposed DF2.
filterCoeffs :: Double -> Double -> Double -> Double -> Double
             -> (Double, Double, Double, Double, Double)
filterCoeffs sr g f0 q sh =
    case shapeOfFloat sh of
        FLowShelf -> lowShelf g f0 sr
        FPeak     -> peak g f0 q sr
        FHighShelf -> highShelf g f0 sr
        FNotch    -> notch f0 q sr
        FBandpass -> bandpass f0 q sr
        FLowpass  -> lowpass f0 q sr
        FHighpass -> highpass f0 q sr
  where
    f0' = clamp (20.0, 0.98 * 0.5 * sr) f0
    g'  = clamp (-24.0, 24.0) g
    q'  = clamp (0.1, 10.0) q
    w0  = 2.0 * pi * f0' / sr
    c   = cos w0
    s   = sin w0
    lowShelf a f0'' _sr =
        let a'  = 10 ** (g' / 40.0)
            b   = sqrt (2.0 * a')
            bb  = 2.0 * sqrt a' * b
            w0' = 2.0 * pi * f0'' / sr
            c'  = cos w0'
            s'  = sin w0'
            a0  = (a' + 1) + (a' - 1) * c' + bb * s'
        in ( a' * ((a' + 1) - (a' - 1) * c' + bb * s') / a0
           , 2.0 * a' * ((a' - 1) - (a' + 1) * c') / a0
           , a' * ((a' + 1) - (a' - 1) * c' - bb * s') / a0
           , -2.0 * ((a' - 1) + (a' + 1) * c') / a0
           , ((a' + 1) + (a' - 1) * c' - bb * s') / a0 )
    highShelf a f0'' _sr =
        let a'  = 10 ** (g' / 40.0)
            b   = 1.0 / sqrt (2.0 * a')
            bb  = 2.0 * sqrt a' * b
            w0' = 2.0 * pi * f0'' / sr
            c'  = cos w0'
            s'  = sin w0'
            a0  = (a' + 1) + (a' - 1) * c' + bb * s'
        in ( a' * ((a' + 1) - (a' - 1) * c' + bb * s') / a0
           , -2.0 * a' * ((a' - 1) + (a' + 1) * c') / a0
           , a' * ((a' + 1) - (a' - 1) * c' - bb * s') / a0
           , -2.0 * ((a' - 1) + (a' + 1) * c') / a0
           , ((a' + 1) + (a' - 1) * c' - bb * s') / a0 )
    peak _ f0'' q'' _sr =
        let a'  = 10 ** (g' / 40.0)
            w0' = 2.0 * pi * f0'' / sr
            c'  = cos w0'
            s'  = sin w0'
            al  = s' / (2.0 * q'')
            a0  = 1.0 + al / a'
        in ( (1.0 + al * a') / a0
           , -2.0 * c' / a0
           , (1.0 - al * a') / a0
           , -2.0 * c' / a0
           , (1.0 - al / a') / a0 )
    notch f0'' q'' _sr =
        let w0' = 2.0 * pi * f0'' / sr
            c'  = cos w0'
            s'  = sin w0'
            al  = s' / (2.0 * q'')
            a0  = 1.0 + al
            n0  = 1.0 / a0
        in ( n0, -2.0 * c' / a0, n0, -2.0 * c' / a0, (1.0 - al) / a0 )
    bandpass f0'' q'' _sr =
        let w0' = 2.0 * pi * f0'' / sr
            c'  = cos w0'
            s'  = sin w0'
            al  = s' / (2.0 * q'')
            a0  = 1.0 + al
            n0  = al / a0
        in ( n0, 0.0, -n0, -2.0 * c' / a0, (1.0 - al) / a0 )
    lowpass f0'' q'' _sr =
        let w0' = 2.0 * pi * f0'' / sr
            c'  = cos w0'
            s'  = sin w0'
            al  = s' / (2.0 * q'')
            a0  = 1.0 + al
            b0  = (1.0 - c') / (2.0 * a0)
        in ( b0, 2.0 * b0, b0, -2.0 * c' / a0, (1.0 - al) / a0 )
    highpass f0'' q'' _sr =
        let w0' = 2.0 * pi * f0'' / sr
            c'  = cos w0'
            s'  = sin w0'
            al  = s' / (2.0 * q'')
            a0  = 1.0 + al
            b0  = (1.0 + c') / (2.0 * a0)
        in ( b0, -2.0 * b0, b0, -2.0 * c' / a0, (1.0 - al) / a0 )

-- | One transposed direct-form-II biquad step: state (x1, x2, y1, y2).
biquadStep :: (Double, Double, Double, Double, Double)
           -> (Double, Double, Double, Double)
           -> Double
           -> (Double, (Double, Double, Double, Double))
biquadStep (b0, b1, b2, a1, a2) (x1, x2, y1, y2) x =
    let y = b0 * x + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
    in (y, (x, x1, y, y1))

-- | Per-voice filter state: 128 slots x 2 stages x 8 doubles
--   (4 biquad delays x1/x2/y1/y2 + 4 smoothed params cutoff/gain/shape/Q).
--   Index: slot*16 + stage*8 + k.  Smoothed params start at 'defFilterParams'
--   so the first block already has the neutral pass-through coefficients.
newVFilterState :: IO (IOUArray Int Double)
newVFilterState = do
    arr <- newArray (0, 2047) (0.0 :: Double)
    let d = defFilterParams
    forM_ [0 .. 127] $ \s ->
        forM_ [0, 1] $ \g -> do
            let b = s * 16 + g * 8
            unsafeWrite arr (b + 4) (fpCutoff d)
            unsafeWrite arr (b + 5) (fpGain d)
            unsafeWrite arr (b + 6) (fpShape d)
            unsafeWrite arr (b + 7) (fpQ d)
    return arr

-- | Per-voice "fresh" flags: 1 while a slot has been audible since its last
--   idle transition.  Used to snap the filter's smoothed params straight to
--   the note's targets on the first audible block — otherwise a note-on
--   would start from a stale/zero state and sweep into the right response,
--   which sounds like a click.
newVFilterFresh :: IO (IOUArray Int Int)
newVFilterFresh = newArray (0, 127) (0 :: Int)

-- | Bundle of the audio callback's mutable state.  One record shared by the
--   real-time callback (read-only view) and the engine / preset-switch code
--   (writes targets).  Each modulatable effect parameter has a target IORef
--   (written by the modulation evaluator) and a smoothed IORef the processor
--   chases with a one-pole lerp per block — no zipper on target jumps.
data SynthFx = SynthFx
    { sfGainRef      :: IORef Double                            -- compressor smoothed gain
    , sfCompTgt      :: IORef (Double, Double, Double, Double)  -- comp targets (thr, ratio, atkSec, relSec)
    , sfCompSm       :: IORef (Double, Double, Double, Double)  -- comp smoothed params
    , sfReverbRef    :: IORef ConvReverb
    , sfAlgRef       :: IORef AlgReverb
    , sfSmpRef       :: IORef Double                            -- global sample counter
    , sfAmpSmoothRef :: IORef (IOUArray Int Double)
    , sfAmpMidRef    :: IORef (IOUArray Int Double)
    , sfWidthRef     :: IORef (Double, Double, Double)          -- widener (aSm, x1, y1)
    , sfWidthTgt     :: IORef Double                            -- widener a target
    , sfDcRef        :: IORef (Double, Double, Double, Double)  -- dc state (xl, yl, xr, yr)
    , sfDcRTgt       :: IORef Double                            -- dc R target
    , sfDcRSm        :: IORef Double                            -- dc R smoothed
    , sfEqRef        :: IORef EqFx                              -- 12-band EQ (C++ FFI)
    , sfChorusRef    :: IORef ChorusFx                          -- trinity chorus
    , sfEnsembleRef  :: IORef EnsembleFx                        -- ensemble
    , sfVFilterRef   :: IORef (IOUArray Int Double)             -- per-voice filter state (2048)
    , sfVFreshRef    :: IORef (IOUArray Int Int)                -- per-voice filter fresh flags (128)
    , sfVoice        :: IORef Voice                             -- active preset (switches)
    , sfRmsRef       :: IORef (Double, Double)                  -- output block RMS (L, R), written per block
    , sfSampleRef    :: IORef (Double, Double)                  -- last output sample (L, R) in [-1,1], raw
    }

-- | Push a voice's FX/reverb base parameters into the live effect targets.
--   Called at startup and on preset switch.  Every processor smooths toward
--   its target, so a switch glides instead of clicking.
pushVoiceToFx :: Voice -> SynthFx -> IO ()
pushVoiceToFx voice fx = do
    let fxP = vFx voice
        rv  = vReverb voice
    eqFx <- readIORef (sfEqRef fx)
    setEqTargets eqFx (concatMap (\b -> [ebFreq b, ebGain b, ebShape b, ebQ b]) (fxEqBands fxP))
    chFx <- readIORef (sfChorusRef fx)
    setChorusTargets chFx
        (fxChorusMix fxP, fxChorusDepth fxP, fxChorusRate fxP)
    esFx <- readIORef (sfEnsembleRef fx)
    setEnsembleTargets esFx
        (fxEnsembleWidth fxP, 0.3)
    writeIORef (sfWidthTgt fx) (fxWiden fxP)
    writeIORef (sfDcRTgt fx)   (fxDcR fxP)
    writeIORef (sfCompTgt fx)
        (fromDBFS (fxCompThr fxP), fxCompRatio fxP, fxCompAtk fxP, fxCompRel fxP)
    -- reverb inserts: algorithmic params + convolution mix
    alg <- readIORef (sfAlgRef fx)
    setAlgParams alg (rvAlg rv)
    rev <- readIORef (sfReverbRef fx)
    setConvMix rev (rvConvDry rv) (rvConvWet rv) (rvConvGain rv)

-- | Render all active voices directly into a PortAudio output buffer.
--   Bulk-reads VoicePool once, renders audio, advances phase per voice,
--   then the FX chain (EQ → chorus → widen → ensemble → DC block → alg
--   reverb → conv reverb → compressor).  The active preset comes from
--   @sfVoice@ (an IORef so preset switches take effect live); @bus@ the
--   read-only CC control bus; @fx@ bundles every mutable effect/state ref.
renderCallback :: VoicePool -> Double -> Int -> Ptr CFloat -> SynthFx -> CtrlBus -> IO ()
renderCallback vp sr nFrames outPtr fx bus = do
    voice <- readIORef (sfVoice fx)
    renderVoices vp sr nFrames outPtr fx bus voice
    -- 5. EQ (12-band + mandatory Nyquist guard; the guard is unconditional,
    --    so this always runs — per-band smart bypass lives in the C++ core)
    eqFx <- readIORef (sfEqRef fx)
    processEq eqFx sr nFrames outPtr
    -- 5a. Trinity chorus (bypass: wet mix ~0 → dry path only)
    chFx <- readIORef (sfChorusRef fx)
    chBp <- chorusBypass chFx
    when (not chBp) $ processChorus chFx sr nFrames outPtr
    -- 5b. Dry-path stereo widening (bypass: a ~0 → allpass is identity)
    wTgt <- readIORef (sfWidthTgt fx)
    when (abs wTgt > 1e-9) $
        widenR (sfWidthRef fx) (sfWidthTgt fx) (nFrames * 2) outPtr
    -- 5c. Ensemble (bypass: width ~0 → no decorrelation; also drops the
    --     default 800-sample shared delay)
    esFx <- readIORef (sfEnsembleRef fx)
    esBp <- ensembleBypass esFx
    when (not esBp) $ processEnsemble esFx sr nFrames outPtr
    -- 5d. DC blocker — kept unconditionally (removes the tanh DC offset
    --     before the reverbs would amplify it).
    dcBlockBuffer (sfDcRef fx) (sfDcRTgt fx) (sfDcRSm fx) (nFrames * 2) outPtr
    -- 6. Algorithmic reverb (bypass: wet ~0 and dry ~1 → passthrough)
    algFx <- readIORef (sfAlgRef fx)
    ap <- getAlgParams algFx
    when (abs (apWet ap) > 1e-9 || abs (apDry ap - 1) > 1e-9) $
        processAlgReverb (sfAlgRef fx) nFrames outPtr
    -- 7. Convolution reverb (bypass: wet ~0 and dry ~1 and gain ~1)
    convFx <- readIORef (sfReverbRef fx)
    (cd, cw, cg) <- getConvMix convFx
    when (abs cw > 1e-9 || abs (cd - 1) > 1e-9 || abs (cg - 1) > 1e-9) $
        processReverb (sfReverbRef fx) nFrames outPtr
    -- 8. Master compressor (bypass: threshold ≥ 0 dBFS → never engages)
    (ct, _, _, _) <- readIORef (sfCompTgt fx)
    when (ct < 0.9999) $
        compressBuffer outPtr (nFrames * 2) sr (sfGainRef fx) (sfCompTgt fx) (sfCompSm fx)
    -- 9. Output RMS (L/R) for the UI meter: one zero-alloc scan per block
    --    (~48 Hz @ fpb=1024), written to sfRmsRef; the last interleaved
    --    sample pair goes to sfSampleRef (raw L/R in [-1,1]) so the Gloss
    --    thread can draw an undersampled waveform trace.
    let scan i !sl !sr' !sl0 !sr0
            | i >= nFrames * 2 = do
                writeIORef (sfRmsRef fx)
                    ( sqrt (sl / fromIntegral nFrames)
                    , sqrt (sr' / fromIntegral nFrames) )
                writeIORef (sfSampleRef fx) (sl0, sr0)
            | otherwise = do
                xl <- peekElemOff outPtr i
                xr <- peekElemOff outPtr (i + 1)
                let !xld = realToFrac xl :: Double
                    !xrd = realToFrac xr :: Double
                scan (i + 2) (sl + xld * xld) (sr' + xrd * xrd) xld xrd
    scan 0 0 0 0 0

-- | Voice-synthesis stage: mix every audible slot into the (zeroed) output
--   buffer and advance envelopes/phase — WITHOUT any FX.  Exported so the
--   full-chain test can drive the stages one by one and monitor levels
--   between them.  Block-constant modulation (LFO / CC snapshot / matrix
--   global rows) is computed here exactly as in the real callback.
renderVoices :: VoicePool -> Double -> Int -> Ptr CFloat -> SynthFx -> CtrlBus -> Voice -> IO ()
renderVoices vp sr nFrames outPtr fx bus voice = do
    let gainRef      = sfGainRef fx
        reverbRef    = sfReverbRef fx
        algRef       = sfAlgRef fx
        smpRef       = sfSmpRef fx
        ampSmoothRef = sfAmpSmoothRef fx
        ampMidRef    = sfAmpMidRef fx
        widthRef     = sfWidthRef fx
        dcRef        = sfDcRef fx
        compTgt      = sfCompTgt fx
        compSm       = sfCompSm fx
        widthTgt     = sfWidthTgt fx
        dcRTgt       = sfDcRTgt fx
        dcRSm        = sfDcRSm fx
        eqRef        = sfEqRef fx
        vfltRef      = sfVFilterRef fx
        vfreshRef    = sfVFreshRef fx
        chorusRef    = sfChorusRef fx
        ensembleRef  = sfEnsembleRef fx
    -- Vibrato / FX base values from the preset (block-constant; the
    -- modulation matrix adds amounts on top below).
    let fxP  = vFx voice
        !vibD = fxVibDepth fxP
        !vibR = fxVibRate  fxP
        vMat  = vMatrix voice
    -- 1. Zero the interleaved output buffer; every voice accumulates into it.
    forM_ [0 .. totalSamples - 1] $ \i ->
        pokeElemOff outPtr i (0.0 :: CFloat)

    -- 2. Block-constant LFOs: sampled at the block start, so the modulation
    --    is continuous across blocks but constant within one (no zipper).
    t0 <- readIORef smpRef
    writeIORef smpRef (t0 + fromIntegral nFrames)
    let !lfoVals = map (\lp -> smoothNoise t0 sr (lpRate lp) (lpSeed lp)) (vLfos voice)
    -- CC bus snapshot, normalised [0,1], read once per block (128 reads are
    -- negligible at one per frame). Adding 8 special slots.
    ccSnap <- forM [0 .. 135] $ \i -> ccNorm bus i
    -- putStrLn $ show ccSnap
    -- Envelope states / levels derived from the preset (block-constant).
    let pE = vEnv voice
        !envDec  = Decay (epDecay pE) (epDecayCurve pE)
        !envSus  = Sustain (epSustainLevel pE)
        !susLvl  = realToFrac (epSustainLevel pE) :: Double
        !ampK    = epAmpSmoothK pE
        -- ENV2 / ENV3 (modulation envelopes) stage states
        pE2 = vEnv2 voice
        !envDec2 = Decay (epDecay pE2) (epDecayCurve pE2)
        !envSus2 = Sustain (epSustainLevel pE2)
        !susLvl2 = realToFrac (epSustainLevel pE2) :: Double
        pE3 = vEnv3 voice
        !envDec3 = Decay (epDecay pE3) (epDecayCurve pE3)
        !envSus3 = Sustain (epSustainLevel pE3)
        !susLvl3 = realToFrac (epSustainLevel pE3) :: Double

    ampSmoothArr <- readIORef ampSmoothRef
    ampMidArr    <- readIORef ampMidRef
    fltArr       <- readIORef vfltRef
    freshArr     <- readIORef vfreshRef
    let !n = nFrames :: Int
        !bT = fromIntegral nFrames / realToFrac sr :: Float
    let VoicePool fp = vp
    -- 2b. Modulation matrix — global rows: aggregate source values (max over
    --     the sounding voices) drive the shared effect-parameter targets.
    --     One scan over all slots, no allocation.
    (maxAmp, maxE2, maxE3, maxNl) <- withForeignPtr fp $ \base -> do
        let go !i !ma !m2 !m3 !mn
                | i >= 128 = return (ma, m2, m3, mn)
                | otherwise = do
                    vp' <- peek (base `plusPtr` (i * 72))
                    let e = vpEnvelopeState vp'
                    if vpFrequency vp' == freqIdleSentinel || e == Idle
                        then go (i + 1) ma m2 m3 mn
                        else do
                            let !age = (t0 - vpOnset vp') / sr
                                !nl  = noteLenAt (vNoteLen voice) age
                            go (i + 1)
                                (max ma (vpAmplitude vp'))
                                (max m2 (vpEnv2Amp vp'))
                                (max m3 (vpEnv3Amp vp'))
                                (max mn nl)
        go 0 0 0 0 0
    -- evaluate the matrix: fold once per target (small list, zero alloc)
    let srcV cc e1 e2 e3 lfos nl = \s -> case s of
            SrcCC c   -> cc !! c
            SrcEnv1   -> e1
            SrcEnv2   -> e2
            SrcEnv3   -> e3
            SrcLfo l  -> lfos !! l
            SrcNoteLen -> nl
        gv = srcV ccSnap maxAmp maxE2 maxE3 lfoVals maxNl
        eval tgt = foldl (\a r -> if mrTarget r == tgt then a + mrAmount r * gv (mrSource r) else a) 0 vMat
        clamp0 = max 0
    -- apply to the shared effect targets: base (preset) + Σ amounts.
    -- The 12-band EQ gets all 48 params (freq/gain/shape/Q per band) so
    -- every EQ parameter is a live modulation target (block-rate).
    eqFx <- readIORef eqRef
    setEqTargets eqFx
        (concat [ [ f + eval (TgtEqBand i EqFreq)
                  , g + eval (TgtEqBand i EqGain)
                  , s + eval (TgtEqBand i EqShape)
                  , q + eval (TgtEqBand i EqQ) ]
                | (i, EqBand f g s q) <- zip [0 :: Int ..] (fxEqBands fxP) ])
    chFx <- readIORef chorusRef
    setChorusTargets chFx
        ( clamp0 $ fxChorusMix fxP + eval TgtChorusMix
        , fxChorusDepth fxP
        , fxChorusRate  fxP )
    esFx <- readIORef ensembleRef
    setEnsembleTargets esFx
        ( max 0 (fxEnsembleWidth fxP + eval TgtEnsembleWidth), 0.3 )
    writeIORef (sfWidthTgt fx) (clamp0 (fxWiden fxP + eval TgtWiden))
    writeIORef (sfDcRTgt fx)   (fxDcR fxP)
    writeIORef (sfCompTgt fx)
        ( fromDBFS (fxCompThr fxP + eval TgtCompThr)
        , fxCompRatio fxP, fxCompAtk fxP, fxCompRel fxP )
    -- reverb targets: alg params (wet/room) + conv mix (dry/wet)
    algFx <- readIORef algRef
    ap <- getAlgParams algFx
    setAlgParams algFx ap { apWet = clamp0 (apWet ap + eval TgtAlgWet)
                          , apRoom = clamp (0.05, 0.98) (apRoom ap + eval TgtAlgRoom) }
    convFx <- readIORef reverbRef
    (cd, cw, cg) <- getConvMix convFx
    setConvMix convFx (clamp0 (cd + eval TgtConvDry)) (clamp0 (cw + eval TgtConvWet)) cg

    -- 3. Per-voice loop: snapshot all 128 slots, render the audible ones.
    withForeignPtr fp $ \base -> do
        let readSlot i = peek (base `plusPtr` (i * 72)) :: IO VoiceParams
        forM_ [0 .. 127] $ \slot -> do
            vpSnap <- readSlot slot
            let !freq = vpFrequency vpSnap
                !amp  = vpAmplitude vpSnap
                !ph   = vpPhase vpSnap
                env   = vpEnvelopeState vpSnap
                -- notelen mod amount: age in seconds through the preset map
                !onset  = vpOnset vpSnap
                !notelen = (t0 - onset) / sr
                !nlVal  = noteLenAt (vNoteLen voice) notelen
            let !active = freq /= freqIdleSentinel && env /= Idle
            if not active
                then do
                    -- idle/released-out slot: zero only the biquad DELAY
                    -- states so the next note-on starts from a clean line;
                    -- keep the smoothed params (they snap to the new note's
                    -- targets on the first audible block via the fresh flag,
                    -- instead of sweeping from zero and clicking)
                    unsafeWrite fltArr (slot * 16 + 0) 0
                    unsafeWrite fltArr (slot * 16 + 1) 0
                    unsafeWrite fltArr (slot * 16 + 2) 0
                    unsafeWrite fltArr (slot * 16 + 3) 0
                    unsafeWrite fltArr (slot * 16 + 8) 0
                    unsafeWrite fltArr (slot * 16 + 9) 0
                    unsafeWrite fltArr (slot * 16 + 10) 0
                    unsafeWrite fltArr (slot * 16 + 11) 0
                    unsafeWrite freshArr slot 0
                else do
                    -- per-voice modulation rows: amp gain / freq ratio / vibrato
                    -- depth / filter params for THIS slot (sources use this slot's
                    -- values)
                    let pv = srcV ccSnap amp (vpEnv2Amp vpSnap) (vpEnv3Amp vpSnap) lfoVals nlVal
                        ev tgt = foldl (\a r -> if mrTarget r == tgt then a + mrAmount r * pv (mrSource r) else a) 0 vMat
                        !ampGain = max 0 (1 + ev TgtAmp)
                        !freqMod = ev TgtFreq
                        !vibD'   = max 0 (vibD + ev TgtVibDepth)
                        -- per-voice filter: base + matrix amount for both stages
                        (fp0, fp1) = vFilter voice
                        !f0c = fpCutoff fp0 + ev (TgtVFilter 0 FCutoff)
                        !f0g = fpGain   fp0 + ev (TgtVFilter 0 FGain)
                        !f0s = fpShape  fp0 + ev (TgtVFilter 0 FShape)
                        !f0q = fpQ      fp0 + ev (TgtVFilter 0 FQ)
                        !f1c = fpCutoff fp1 + ev (TgtVFilter 1 FCutoff)
                        !f1g = fpGain   fp1 + ev (TgtVFilter 1 FGain)
                        !f1s = fpShape  fp1 + ev (TgtVFilter 1 FShape)
                        !f1q = fpQ      fp1 + ev (TgtVFilter 1 FQ)
                    let !step0 = freq / sr :: Double
    
                    -- 3a. Two-stage cascade smoothing: s1 (fast stage) chases the
                    -- target, s2 (output) chases s1.  The state is updated
                    -- unconditionally so the output keeps gliding toward the
                    -- target even during fast envelope decays; rendering (and the
                    -- per-block ramp) only runs while anything is audible.
                    s2 <- unsafeRead ampSmoothArr slot
                    s1 <- unsafeRead ampMidArr slot
                    -- s1 always tracks the target quickly so it never holds a
                    -- stale lag; only the output stage s2 uses the slow aftertouch
                    -- smoothing, and only while sustaining.  During envelope
                    -- stages s2 also tracks quickly so the voice is inaudible
                    -- before it is dropped — no stuck level, no click.
                    let !k1     = 0.5
                        !k2     = case env of
                                    Sustain _ -> ampK
                                    _         -> 0.5
                        !s1'    = s1 + k1 * (amp - s1)
                        !s2'    = s2 + k2 * (s1' - s2)
                        !audible = amp /= 0 || s2 > 0.0001
                    when audible $ do
                        -- 3b. Per-voice filters: block-constant coefficients.
                        --     Targets = base + matrix amounts, clamped.  On the
                        --     FIRST audible block after an idle transition the
                        --     smoothed params SNAP to the targets (fresh flag),
                        --     so a note-on starts with the correct response;
                        --     otherwise they one-pole lerp (k=0.3) so modulated
                        --     sweeps glide instead of zippering.
                        let !b0    = slot * 16
                            !fNy  = 0.98 * 0.5 * sr
                            !tC0  = clamp (20.0, fNy) f0c
                            !tG0  = clamp (-24.0, 24.0) f0g
                            !tS0  = clamp (0.0, 1.0) f0s
                            !tQ0  = clamp (0.1, 10.0) f0q
                            !tC1  = clamp (20.0, fNy) f1c
                            !tG1  = clamp (-24.0, 24.0) f1g
                            !tS1  = clamp (0.0, 1.0) f1s
                            !tQ1  = clamp (0.1, 10.0) f1q
                        fresh <- unsafeRead freshArr slot
                        smC0 <- unsafeRead fltArr (b0 + 4)
                        smG0 <- unsafeRead fltArr (b0 + 5)
                        smS0 <- unsafeRead fltArr (b0 + 6)
                        smQ0 <- unsafeRead fltArr (b0 + 7)
                        smC1 <- unsafeRead fltArr (b0 + 12)
                        smG1 <- unsafeRead fltArr (b0 + 13)
                        smS1 <- unsafeRead fltArr (b0 + 14)
                        smQ1 <- unsafeRead fltArr (b0 + 15)
                        let !kSm   = 0.3
                            -- snap (fresh) or lerp (steady)
                            !c0'  = if fresh == 0 then tC0 else smC0 + kSm * (tC0 - smC0)
                            !g0'  = if fresh == 0 then tG0 else smG0 + kSm * (tG0 - smG0)
                            !s0'  = if fresh == 0 then tS0 else smS0 + kSm * (tS0 - smS0)
                            !q0'  = if fresh == 0 then tQ0 else smQ0 + kSm * (tQ0 - smQ0)
                            !c1'  = if fresh == 0 then tC1 else smC1 + kSm * (tC1 - smC1)
                            !g1'  = if fresh == 0 then tG1 else smG1 + kSm * (tG1 - smG1)
                            !s1s  = if fresh == 0 then tS1 else smS1 + kSm * (tS1 - smS1)
                            !q1'  = if fresh == 0 then tQ1 else smQ1 + kSm * (tQ1 - smQ1)
                            !c0   = filterCoeffs sr g0' c0' q0' s0'
                            !c1   = filterCoeffs sr g1' c1' q1' s1s
                        unsafeWrite fltArr (b0 + 4) c0'
                        unsafeWrite fltArr (b0 + 5) g0'
                        unsafeWrite fltArr (b0 + 6) s0'
                        unsafeWrite fltArr (b0 + 7) q0'
                        unsafeWrite fltArr (b0 + 12) c1'
                        unsafeWrite fltArr (b0 + 13) g1'
                        unsafeWrite fltArr (b0 + 14) s1s
                        unsafeWrite fltArr (b0 + 15) q1'
                        when (fresh == 0) $ unsafeWrite freshArr slot 1
                        -- 3c. Sample loop: linear ramp s2 → s2' across the block
                        -- (block boundary stays continuous, no click).  Each
                        -- sample is produced by the pure core synthSample with
                        -- the block's LFO values and CC snapshot in its context,
                        -- then passed through the two per-voice biquads before
                        -- joining the mix.
                        let !delta = (s2' - s2) / fromIntegral nFrames
                        forM_ [0 .. n - 1] (\f -> do
                            let !sf = fromIntegral f :: Double
                                !tAbs = t0 + sf
                                !noise = detNoise (fromIntegral (round t0) + f) slot
                                !noiseW = detNoiseW (fromIntegral (round t0) + f) slot
                                -- amplitude gain from the modulation matrix
                                !af = (s2 + delta * fromIntegral (f + 1)) * ampGain
                                -- pitch: matrix freq ratio × vibrato (per-sample)
                                !step = step0 * (1 + freqMod) * vibRatio vibD' vibR sr tAbs
                                !ctx = SynthCtx
                                    { scFreq    = freq
                                    , scPhase   = ph + sf * step
                                    , scCC      = ccSnap
                                    , scLfos    = lfoVals
                                    , scAmp     = af
                                    , scEnv2    = vpEnv2Amp vpSnap
                                    , scEnv3    = vpEnv3Amp vpSnap
                                    , scNoise   = noise
                                    , scNoise'  = noiseW
                                    , scNoteLen = nlVal
                                    }
                                !sRaw = vSample voice ctx
                            -- per-voice filter stage 0 and 1 (state per slot)
                            st0x1 <- unsafeRead fltArr (b0 + 0)
                            st0x2 <- unsafeRead fltArr (b0 + 1)
                            st0y1 <- unsafeRead fltArr (b0 + 2)
                            st0y2 <- unsafeRead fltArr (b0 + 3)
                            st1x1 <- unsafeRead fltArr (b0 + 8)
                            st1x2 <- unsafeRead fltArr (b0 + 9)
                            st1y1 <- unsafeRead fltArr (b0 + 10)
                            st1y2 <- unsafeRead fltArr (b0 + 11)
                            let !(s1v, (x0a, x0b, y0a, y0b)) =
                                    biquadStep c0 (st0x1, st0x2, st0y1, st0y2) sRaw
                                !(s2v, (x1a, x1b, y1a, y1b)) =
                                    biquadStep c1 (st1x1, st1x2, st1y1, st1y2) s1v
                                !s = realToFrac s2v :: CFloat
                            unsafeWrite fltArr (b0 + 0) x0a
                            unsafeWrite fltArr (b0 + 1) x0b
                            unsafeWrite fltArr (b0 + 2) y0a
                            unsafeWrite fltArr (b0 + 3) y0b
                            unsafeWrite fltArr (b0 + 8) x1a
                            unsafeWrite fltArr (b0 + 9) x1b
                            unsafeWrite fltArr (b0 + 10) y1a
                            unsafeWrite fltArr (b0 + 11) y1b
                            -- Accumulate into both channels (stereo, same signal).
                            curL <- peekElemOff outPtr (f * 2)
                            pokeElemOff outPtr (f * 2)     (curL + s)
                            curR <- peekElemOff outPtr (f * 2 + 1)
                            pokeElemOff outPtr (f * 2 + 1) (curR + s))
                    unsafeWrite ampSmoothArr slot s2'
                    unsafeWrite ampMidArr slot s1'
    
                    -- 4. Envelope advance (ENV1 = volume).  Every env write is
                    --    guarded by a re-read: the MIDI thread (mainLoop) may
                    --    have just triggered Attack/Release/Idle on this voice,
                    --    and overwriting it here would either keep a released
                    --    note alive or cut a new one — both sound as clicks.
                    --    If the field changed since our block snapshot, skip
                    --    the write and let the new state win.
                    let !newPh = ph + fromIntegral n * step0 * (1 + freqMod)
                    advanceEnvAt base slot 24 8 (Just 16) env amp newPh bT susLvl s2' envDec envSus
    
            -- 4b. ENV2 / ENV3 advance: independent modulation envelopes, run for
            --     every non-idle slot (NOT gated by ENV1/audibility, so their
            --     outputs keep moving through release).  Their levels are the
            --     env2amp/env3amp fields consumed by the modulation matrix.
            forM_ [0 .. 127] $ \slot -> do
                vp23 <- readSlot slot
                let !e2 = vpEnv2State vp23
                    !e3 = vpEnv3State vp23
                    !a2 = vpEnv2Amp vp23
                    !a3 = vpEnv3Amp vp23
                when (e2 /= Idle) $
                    advanceEnvAt base slot 32 40 Nothing e2 a2 0 bT susLvl2 a2 envDec2 envSus2
                when (e3 /= Idle) $
                    advanceEnvAt base slot 48 56 Nothing e3 a3 0 bT susLvl3 a3 envDec3 envSus3
  where
    totalSamples = nFrames * 2

-- | Advance one envelope state machine located at @envOff@ (24 = ENV1,
--   32 = ENV2, 48 = ENV3), writing its output level to @ampOff@ (8/40/56).
--   @phaseOff@ is the oscillator phase field (16) for ENV1 — pass 'Nothing'
--   for the modulation envelopes, which must not touch the synth thread's
--   phase.  Every write is guarded by a re-read of the env field: the MIDI
--   thread may have just triggered a new state and must win.  @curOut@ is
--   the current output level (deltaPower start point); @gate@ is the
--   "inaudible" threshold that ends Release (ENV1 passes the smoothed
--   output s2', the mod envelopes pass their own output).
advanceEnvAt
    :: Ptr VoiceParams -> Int    -- base pointer, slot
    -> Int -> Int -> Maybe Int   -- env offset, amp offset, phase offset
    -> EnvelopeState             -- snapshot env (guard)
    -> Double                    -- curOut: current output level
    -> Double                    -- newPh: precomputed phase advance (ignored when phaseOff = Nothing)
    -> Float                     -- bT: block time (seconds)
    -> Double                    -- susLvl: sustain level target
    -> Double                    -- gate: Release end threshold
    -> EnvelopeState             -- envDec: Decay handover state
    -> EnvelopeState             -- envSus: Sustain handover state
    -> IO ()
advanceEnvAt base slot envOff ampOff phaseOff env curOut newPh bT susLvl gate envDec envSus = do
    let pokeOff off v = pokeByteOff (base `plusPtr` (slot * 72)) off v
        pokePhase = case phaseOff of
            Just off -> pokeOff off newPh
            Nothing  -> return ()
        guardWrite act = do
            curEnv <- peek (base `plusPtr` (slot * 72 + envOff)) :: IO EnvelopeState
            when (curEnv == env) act
    case env of
        -- Attack: ramp toward 1.0; when the stage duration is spent, hand
        -- over to Decay at full level.
        Attack d c
            | d <= bT -> guardWrite $ do
                pokePhase
                pokeOff ampOff (1.0 :: Double)
                pokeOff envOff envDec
            | otherwise -> guardWrite $ do
                let !newD = d - bT
                    !newA = clamp (0, 1) $ curOut + deltaPower curOut 1.0 (realToFrac c) (realToFrac (bT / d)) curOut
                pokePhase
                pokeOff ampOff newA
                pokeOff envOff (Attack newD c)
        -- Decay: glide toward the sustain level, then settle into Sustain.
        Decay d c
            | d <= bT -> guardWrite $ do
                pokePhase
                pokeOff ampOff susLvl
                pokeOff envOff envSus
            | otherwise -> guardWrite $ do
                let !newD = d - bT
                    !newA = clamp (0, 1) $ curOut + deltaPower curOut susLvl (realToFrac c) (realToFrac (bT / d)) curOut
                pokePhase
                pokeOff ampOff newA
                pokeOff envOff (Decay newD c)
        -- Sustain: keep the current level — ENV1 lets aftertouch control it
        -- from here on (writing every block would fight aftertouch and click).
        Sustain _ -> pokePhase
        -- Release: curve decay while d remains; once spent, keep an
        -- exponential fade-out (-50%/block) instead of hard-cutting.  The
        -- Idle gate is driven by the *actual output* (smoothed s2' for
        -- ENV1, env2amp/env3amp for the mod envelopes), not the envelope
        -- target — with slow smoothing the output can still be loud when
        -- the target reaches 0, and cutting then clicks.
        Release d c
            | gate < 0.0005 -> guardWrite $ do
                -- Fully inaudible: drop the envelope.
                pokePhase
                pokeOff ampOff (0.0 :: Double)
                pokeOff envOff Idle
            | otherwise -> guardWrite $ do
                let !newD = max 0 (d - bT)
                    !newA = if d > bT
                                then clamp (0, 1) $ curOut + deltaPower curOut 0.0 (realToFrac c) (realToFrac (bT / d)) curOut
                                else curOut * 0.5
                pokePhase
                pokeOff ampOff newA
                pokeOff envOff (Release newD c)
        Idle -> return ()

-- | First-order DC-blocking high-pass on both channels:
--   y[n] = x[n] − x[n−1] + R·y[n−1].  State (prev input, prev output) per
--   channel persists across blocks in @stRef@.  The coefficient R is
--   live-modulatable: @tgtRef@ holds the target, @smRef@ the smoothed value
--   chased with a one-pole lerp per block.
dcBlockBuffer :: IORef (Double, Double, Double, Double) -> IORef Double -> IORef Double -> Int -> Ptr CFloat -> IO ()
dcBlockBuffer stRef tgtRef smRef totalSamples outPtr = do
    (xl, yl, xr, yr) <- readIORef stRef
    t <- readIORef tgtRef
    s <- readIORef smRef
    let !r   = s + 0.1 * (t - s)
        go !i !xp !yp !xq !yq
            | i >= totalSamples = writeIORef stRef (xp, yp, xq, yq)
            | otherwise = do
                xL <- peekElemOff outPtr i
                xR <- peekElemOff outPtr (i + 1)
                let yL = realToFrac xL - xp + r * yp
                    yR = realToFrac xR - xq + r * yq
                pokeElemOff outPtr i     (realToFrac yL)
                pokeElemOff outPtr (i + 1) (realToFrac yR)
                go (i + 2) (realToFrac xL) yL (realToFrac xR) yR
    writeIORef smRef r
    go 0 xl yl xr yr

-- | First-order allpass on the R channel only: y = a·x + x1 − a·y1.
--   State (prev input, prev output) persists across blocks in @stRef@.
--   The coefficient a is live-modulatable: @tgtRef@ target, smoothed in the
--   state tuple (aSm, x1, y1) with a one-pole lerp per block.  a = 0 leaves
--   the R channel untouched apart from a single-sample delay (L=R again).
widenR :: IORef (Double, Double, Double) -> IORef Double -> Int -> Ptr CFloat -> IO ()
widenR stRef tgtRef totalSamples outPtr = do
    (aOld, x1, y1) <- readIORef stRef
    aTgt <- readIORef tgtRef
    let !a = aOld + 0.1 * (aTgt - aOld)
        go !i !xp !yp
            | i >= totalSamples = writeIORef stRef (a, xp, yp)
            | otherwise = do
                x <- peekElemOff outPtr (i + 1)
                let y = a * realToFrac x + xp - a * yp
                pokeElemOff outPtr (i + 1) (realToFrac y)
                go (i + 2) (realToFrac x) y
    go 0 x1 y1

-- | Feed-forward compressor post-pass (stereo-linked gain).
--   Detects the peak envelope of both channels, derives a target gain
--   (1.0 below threshold, heavy compression above), and smooths it with a
--   one-pole filter — fast attack, slow release — so the gain trajectory
--   continues across blocks via @gainRef@ (no block-boundary zipper).
--   The threshold/ratio/attack/release parameters are live-modulatable:
--   @tgtRef@ holds (thr, ratio, atkSec, relSec) targets, @smRef@ the
--   smoothed values chased per block.
compressBuffer :: Ptr CFloat -> Int -> Double -> IORef Double -> IORef (Double, Double, Double, Double) -> IORef (Double, Double, Double, Double) -> IO ()
compressBuffer outPtr totalSamples sr gainRef tgtRef smRef = do
    g <- readIORef gainRef
    (thrT, ratioT, atkT, relT) <- readIORef tgtRef
    (thr, ratio, atkS, relS)   <- readIORef smRef
    let !k = 0.1
        !thr'   = thr   + k * (thrT - thr)
        !ratio' = ratio + k * (ratioT - ratio)
        !atkS'  = atkS  + k * (atkT - atkS)
        !relS'  = relS  + k * (relT - relS)
        aAtk   = 1.0 - exp (- (1.0 / (atkS' * sr)))
        aRel   = 1.0 - exp (- (1.0 / (relS' * sr)))
        go !i !gain
            | i >= totalSamples = writeIORef gainRef gain
            | otherwise = do
                -- stereo-linked detection: max |L| |R|
                xL <- peekElemOff outPtr i
                xR <- peekElemOff outPtr (i + 1)
                let env = max (abs (realToFrac xL)) (abs (realToFrac xR)) :: Double
                    -- >threshold above → heavy compression (ratio:1)
                    targetGain
                        | env > thr' = (thr' + (env - thr') / ratio') / env
                        | otherwise  = 1.0
                    -- attack when reducing gain, release when recovering
                    alpha  = if targetGain < gain then aAtk else aRel
                    gain'  = gain + alpha * (targetGain - gain)
                    yL     = realToFrac (realToFrac xL * gain') :: CFloat
                    yR     = realToFrac (realToFrac xR * gain') :: CFloat
                pokeElemOff outPtr i     yL
                pokeElemOff outPtr (i + 1) yR
                go (i + 2) gain'
    writeIORef smRef (thr', ratio', atkS', relS')
    go 0 g

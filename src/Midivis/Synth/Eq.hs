{-# LANGUAGE BangPatterns #-}
-- | Twelve-band parametric EQ + mandatory Nyquist guard, stereo.
--
--   The DSP core lives in C++ (cbits/Eq12.cpp) for speed; Haskell owns the
--   shared parameter block (48 floats: 12 bands x (freq, gain dB, shape
--   0..1, Q)) as a pinned 'IOVector' and hands its pointer to the C++ side
--   once per block — zero copy across the FFI boundary.  The C++ state
--   (biquad delays + smoothed params) stays opaque behind a 'ForeignPtr'
--   finalised by @eq12_free@.
--
--   The guard band (8th-order Butterworth lowpass at 0.98 x Nyquist) is
--   unconditional, so this EQ always runs — there is no whole-EQ bypass;
--   per-band smart bypass lives inside the C++ core (gain-type shapes with
--   ~0 dB gain are skipped).
module Midivis.Synth.Eq
    ( EqFx
    , newEqFx
    , setEqTargets
    , processEq
    ) where

import Prelude
import Data.Vector.Storable.Mutable (IOVector, new, write, unsafeWith)
import Foreign.C.Types (CFloat)
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

-- | The EQ: an opaque C++ state plus the 48-float shared parameter block.
data EqFx = EqFx
    { eqState  :: !(ForeignPtr ())   -- ^ C++ biquad/smoothing state
    , eqParams :: !(IOVector CFloat) -- ^ 12 bands x (freq, gain, shape, Q)
    }

-- | Allocate the EQ (neutral: every band flat / peak at 1 kHz, so the 12
--   variable bands are all smart-bypassed and only the guard lowpass runs).
newEqFx :: IO EqFx
newEqFx = do
    st  <- eq12New
    fst <- newForeignPtr eq12FreePtr st
    ps  <- new 48
    return $ EqFx fst ps

-- | Write the 48 band parameters (12 x (freq Hz, gain dB, shape 0..1, Q)).
--   Called once per block from the render callback.
setEqTargets :: EqFx -> [Double] -> IO ()
setEqTargets eq ps = go 0 ps
  where
    go _ [] = return ()
    go i (v : vs) = write (eqParams eq) i (realToFrac v) >> go (i + 1) vs

-- | Process a stereo-interleaved CFloat buffer in place.
processEq :: EqFx -> Double -> Int -> Ptr CFloat -> IO ()
processEq eq sr nFrames outPtr =
    withForeignPtr (eqState eq) $ \st ->
        unsafeWith (eqParams eq) $ \params ->
            eq12Process st params outPtr nFrames sr

foreign import ccall unsafe "eq12_new" eq12New :: IO (Ptr ())
foreign import ccall unsafe "&eq12_free" eq12FreePtr :: FinalizerPtr ()
foreign import ccall unsafe "eq12_process"
    eq12Process :: Ptr () -> Ptr CFloat -> Ptr CFloat -> Int -> Double -> IO ()

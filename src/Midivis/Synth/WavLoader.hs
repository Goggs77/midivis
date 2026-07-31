{-# LANGUAGE BangPatterns #-}
-- | Minimal WAV loader for impulse responses.
--   Supports PCM 16/24/32-bit and IEEE float 32-bit; stereo is mixed to mono.
--   Returns normalised samples in [-1, 1].
module Midivis.Synth.WavLoader
    ( loadWavMono
    ) where

import Prelude
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word (Word16, Word32)
import Data.Int (Int16, Int32)
import Data.Bits (shiftL, (.|.))

-- | Load a WAV file and return its samples as mono [-1, 1] doubles.
loadWavMono :: BS.ByteString -> IO [Double]
loadWavMono file = do
    return $ runGet getWav (BL.fromStrict file)

getWav :: Get [Double]
getWav = do
    _riff <- getByteString 4          -- "RIFF"
    _sz   <- getWord32le
    _wave <- getByteString 4          -- "WAVE"
    _fmt  <- getByteString 4          -- "fmt "
    fmtSz <- getWord32le
    aFormat <- getWord16le            -- 1 = PCM, 3 = IEEE float
    nCh     <- getWord16le
    _srate  <- getWord32le
    _brate  <- getWord32le
    _align  <- getWord16le
    bits    <- getWord16le
    when (fmtSz > 16) $ skip (fromIntegral fmtSz - 16)
    getChunks nCh aFormat bits

-- | Walk the remaining chunks until we find the "data" chunk.
getChunks :: Word16 -> Word16 -> Word16 -> Get [Double]
getChunks nCh aFormat bits = go
  where
    go = do
        tag <- getByteString 4
        sz  <- getWord32le
        if tag == C8.pack "data"
            then getByteString (fromIntegral sz) >>= \raw ->
                     return $ decodeSamples nCh aFormat bits raw
            else skip (fromIntegral sz) >> go

decodeSamples :: Word16 -> Word16 -> Word16 -> BS.ByteString -> [Double]
decodeSamples nCh aFormat bits raw =
    let n = fromIntegral nCh :: Int
        vals = decodeFormat aFormat bits raw
    in if n <= 1 then vals else mixMono n vals

mixMono :: Int -> [Double] -> [Double]
mixMono _ [] = []
mixMono n xs =
    let (chunk, tl) = splitAt n xs
    in if length chunk < n
           then []
           else sum chunk / fromIntegral n : mixMono n tl

decodeFormat :: Word16 -> Word16 -> BS.ByteString -> [Double]
decodeFormat aFormat bits raw
    | aFormat == 3 = decodeF32 raw
    | bits == 16   = decode16 raw
    | bits == 24   = decode24 raw
    | bits == 32   = decode32 raw
    | otherwise    = []

decode16 :: BS.ByteString -> [Double]
decode16 bs
    | BS.length bs < 2 = []
    | otherwise =
        let (b0, b1) = BS.splitAt 2 bs
            v = runGet getInt16le (BL.fromStrict b0)
        in fromIntegral v / 32768.0 : decode16 b1

decode24 :: BS.ByteString -> [Double]
decode24 bs
    | BS.length bs < 3 = []
    | otherwise =
        let (b0, b1) = BS.splitAt 3 bs
            bytes = BS.unpack b0
            s0 = fromIntegral (bytes !! 0) :: Int
            s1 = fromIntegral (bytes !! 1) :: Int
            s2 = fromIntegral (bytes !! 2) :: Int
            signed = if s2 < 128
                         then (s2 `shiftL` 16) .|. (s1 `shiftL` 8) .|. s0
                         else ((s2 - 256) `shiftL` 16) .|. (s1 `shiftL` 8) .|. s0
        in fromIntegral signed / 8388608.0 : decode24 b1

decode32 :: BS.ByteString -> [Double]
decode32 bs
    | BS.length bs < 4 = []
    | otherwise =
        let (b0, b1) = BS.splitAt 4 bs
            v = runGet getInt32le (BL.fromStrict b0)
        in fromIntegral v / 2147483648.0 : decode32 b1

decodeF32 :: BS.ByteString -> [Double]
decodeF32 bs
    | BS.length bs < 4 = []
    | otherwise =
        let (b0, b1) = BS.splitAt 4 bs
            v = runGet getFloatle (BL.fromStrict b0)
        in realToFrac v : decodeF32 b1

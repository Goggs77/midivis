{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Midivis.World
import Midivis.System.ExampleRenderer 
import Midivis.Synth.Engine
import Midivis.Synth.Types
import Midivis.Synth.Render (renderCallback)
import Sound.PortAudio
import qualified Sound.PortAudio.Base as Base
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.IORef (newIORef)
import System.IO (hPutStrLn, stderr)
import Foreign.C.Types (CFloat)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (poke, sizeOf)
import Foreign.Ptr (plusPtr)
import Control.Monad (forM_, forM, forever)
import Data.List (maximumBy)
import Data.Function (on)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
    -- === 1. Create shared state ===
    midiTQue <- newTQueueIO
    w0TVar <- newTVarIO $ initWorld midiTQue
    let fpb = 256
        nch = 2
        sr  = defSampleRate

    -- === 2. Start synth engine (forkIO mainLoop) ===
    vp <- initSynth w0TVar midiTQue
    hPutStrLn stderr "[Main] Synth engine started"

    -- === 3. Start PortAudio output (forkIO thread, callback mode) ===
    _ <- forkIO $ do
        hPutStrLn stderr "[Audio] Initializing PortAudio..."
        r <- withPortAudio $ do
            -- probe devices 0..4 with a quick test
            let probe devIdx = do
                    let outParams = Just (StreamParameters (fromIntegral devIdx) (fromIntegral nch) (Base.PaTime 0.1))
                    result <- withStream
                        Nothing
                        outParams
                        sr
                        (Just fpb)
                        [ClipOff]
                        Nothing
                        Nothing
                        $ \(strm :: Stream CFloat CFloat) -> do
                            merr <- startStream strm
                            case merr of
                                Just err -> pure $ Left err
                                Nothing -> do
                                    allocaArray (fpb * fromIntegral nch) $ \bufPtr -> do
                                        bufFP <- newForeignPtr_ bufPtr
                                        withForeignPtr bufFP $ \ptr ->
                                            forM_ [0 .. fpb - 1] $ \i -> do
                                                let s = sin (2 * pi * fromIntegral i * 440.0 / realToFrac sr) * 0.5 :: CFloat
                                                    offL = i * 2
                                                    offR = i * 2 + 1
                                                poke (ptr `plusPtr` (offL * sizeOf (undefined :: CFloat))) s
                                                poke (ptr `plusPtr` (offR * sizeOf (undefined :: CFloat))) s
                                        merr2 <- writeStream strm (fromIntegral fpb) bufFP
                                        _ <- stopStream strm
                                        pure $ case merr2 of
                                            Nothing -> Right (fromIntegral devIdx)
                                            Just e  -> Left e
                    case result of
                        Right dev -> pure (Right dev)
                        Left _ | devIdx < 4 -> probe (devIdx + 1)
                        Left err -> pure $ Left err
            devResult <- probe 0
            case devResult of
                Left err -> do
                    hPutStrLn stderr $ "[Audio] No working device: " ++ show err
                    pure $ Left err
                Right devIdx -> do
                    hPutStrLn stderr $ "[Audio] Using device " ++ show devIdx
                    let outParams = Just (StreamParameters (fromIntegral devIdx) (fromIntegral nch) (Base.PaTime 0.1))
                    phVar <- newIORef (1.0 :: Double)

                    let callback :: StreamCallback CFloat CFloat
                        callback _time _flags nFrames _inp outPtr = do
                            renderCallback vp sr (fromIntegral nFrames) outPtr phVar
                            return Continue

                    withStream
                        Nothing
                        outParams
                        sr
                        (Just fpb)
                        [ClipOff]
                        (Just callback)
                        Nothing
                        $ \(strm :: Stream CFloat CFloat) -> do
                            serr <- startStream strm
                            case serr of
                                Just e -> do
                                    hPutStrLn stderr $ "[Audio] startStream error: " ++ show e
                                    return $ Left e
                                Nothing -> do
                                    hPutStrLn stderr "[Audio] PortAudio stream active (callback)"
                                    forever $ threadDelay maxBound
        case r of
            Left err -> hPutStrLn stderr $ "[Audio] PortAudio error: " ++ show err
            Right _  -> hPutStrLn stderr "[Audio] PortAudio finished"

    -- Give threads time to initialise
    threadDelay 100000

    -- === 4. Start Gloss visualization (blocks main thread) ===
    hPutStrLn stderr "[Main] Starting Gloss visualization..."
    drawExampleRelative w0TVar

    hPutStrLn stderr "[Main] Shutdown"

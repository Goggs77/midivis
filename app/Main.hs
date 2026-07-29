module Main where

import Midivis.World
import Midivis.System.ExampleRenderer 
import Sound.PortAudio
import Control.Concurrent.STM

main :: IO ()
main = do
    {-
    e <- withPortAudio $ do
        let sampleRate = 48000
            numChannels = 2
            framesPerBuffer = Nothing
    
        withDefaultStream
            0
            numChannels
            sampleRate
            framesPerBuffer
            Nothing
            Nothing
            $ \stream  -> do
                me1 <- startStream stream
                _ <- case me1 of
                    Nothing -> do
                        
                    Just e1 -> error $ show e1
                me2 <- stopStream stream
                case me2 of
                    Nothing -> return $ Right ()
                    Just e2 -> error $ show e2
    case e of
        Left e1 -> error $ show e1-}
    midiTQue <- newTQueueIO
    --Warning: TQueue with no consumer is prone to memory leak
    w0TVar <- newTVarIO $ initWorld midiTQue
    drawExampleRelative w0TVar


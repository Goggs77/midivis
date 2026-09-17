{-# LANGUAGE CPP #-}
module Graphics.Gloss.Interface.Environment where

import Data.IORef (newIORef)

import qualified Graphics.Gloss.Internals.Interface.Backend.Types as Backend.Types
import Graphics.Gloss.Internals.Interface.Backend (defaultBackendState)

#ifdef WITHGLUT
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT (($=), get)
#endif

-- | Set the title of the currently open window.
--
--   Must be called after the window has been created — typically from a
--   simulation step, an event handler, or the controller callback of
--   'Graphics.Gloss.Interface.IO.Display.displayIO'.  With the default
--   GLUT backend this maps to @glutSetWindowTitle@.
setWindowTitle :: String -> IO ()
#ifdef WITHGLUT
setWindowTitle s = GLUT.windowTitle $= s
#else
setWindowTitle _ = error "setWindowTitle: only supported by the GLUT backend"
#endif

-- | Get the size of the currently open window, in pixels @(width, height)@.
--
--   Must be called after the window has been created — typically from a
--   simulation step, an event handler, or the controller callback.  With
--   the default GLUT backend this reads the current GLUT window size.
getWindowSize :: IO (Int, Int)
#ifdef WITHGLUT
getWindowSize = do
    GLUT.Size sizeX sizeY <- get GLUT.windowSize
    return (fromEnum sizeX, fromEnum sizeY)
#else
getWindowSize = error "getWindowSize: only supported by the GLUT backend"
#endif

-- | Get the size of the screen, in pixels.
--
--   This will be the size of the rendered gloss image when
--   fullscreen mode is enabled.
--
getScreenSize :: IO (Int, Int)
getScreenSize = do
       backendStateRef <- newIORef defaultBackendState
       Backend.Types.initializeBackend backendStateRef False
       Backend.Types.getScreenSize backendStateRef


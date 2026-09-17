{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Rendering.Common
        ( gf, gsizei
        , withModelview
        , withClearBuffer)
where
import Unsafe.Coerce
import Graphics.Gloss.Internals.Data.Color
import Graphics.Gloss.Internals.Rendering.Color
import Graphics.Gloss.Internals.Rendering.Framebuffer
import Graphics.Rendering.OpenGL               (($=))
import qualified Graphics.Rendering.OpenGL.GL   as GL
import Data.IORef
import System.IO.Unsafe


-- | The OpenGL library doesn't seem to provide a nice way convert
--      a Float to a GLfloat, even though they're the same thing
--      under the covers.
--
--  Using realToFrac is too slow, as it doesn't get fused in at
--      least GHC 6.12.1
--
gf :: Float -> GL.GLfloat
gf x = unsafeCoerce x
{-# INLINE gf #-}


-- | Used for similar reasons to above
gsizei :: Int -> GL.GLsizei
gsizei x = unsafeCoerce x
{-# INLINE gsizei #-}


-- | Pool for the supersampling render target (shared across frames,
--   rebuilt on window resize).
ssaaPool :: IORef (Maybe RT)
ssaaPool = unsafePerformIO (newIORef Nothing)
{-# NOINLINE ssaaPool #-}

-- | Set up the OpenGL rendering context for orthographic projection and run an
--   action to draw the model.
--
--   Supersampling: when 'ssaaFactor' > 1, the scene is rendered into an
--   offscreen FBO at a higher resolution, then composited back to the
--   window with linear filtering (downsampling = antialiasing).  The
--   orthographic projection always uses the window coordinate system, so
--   picture coordinates are unaffected.  With 'ssaaFactor' = 1 the
--   pipeline is bypassed and the scene renders directly to the window
--   (identical behaviour, zero overhead).
withModelview
        :: (Int, Int)  -- ^ Width and height of window.
        -> IO ()       -- ^ Action to perform.
        -> IO ()

withModelview size@(sizeX, sizeY) action
 = do
        if ssaaFactor <= 1
            then renderDirect size action
            else do
                rt <- getRT ssaaPool size
                withRenderTarget rt size action
                blitToWindow rt size
  where
    -- direct render: set the viewport to the window and run the action
    -- under the window-coordinate ortho projection
    renderDirect (sizeX', sizeY') action' = do
        let (sx, sy) = (fromIntegral sizeX' / 2, fromIntegral sizeY' / 2)
        GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral sizeX') (fromIntegral sizeY'))
        GL.matrixMode   $= GL.Projection
        GL.preservingMatrix
         $ do
                GL.loadIdentity
                GL.ortho (-sx) sx (-sy) sy 0 (-100)
                GL.matrixMode   $= GL.Modelview 0
                action'
                GL.matrixMode   $= GL.Projection
        GL.matrixMode   $= GL.Modelview 0


-- | Clear the OpenGL buffer with the given background color and run
--   an action to draw the model.
withClearBuffer
        :: Color        -- ^ Background color
        -> IO ()        -- ^ Action to perform
        -> IO ()

withClearBuffer clearColor action
 = do
        -- initialization (done every time in this case)
        -- we don't need the depth buffer for 2d.
        GL.depthFunc    GL.$= Just GL.Always

        -- always clear the buffer to white
        GL.clearColor   GL.$= glColor4OfColor clearColor

        -- on every loop
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)

        action


{-# OPTIONS_HADDOCK hide #-}

-- | Offscreen framebuffer rendering pipeline.
--
--   A reusable, pooled render target (FBO + colour texture) plus the
--   primitives to render a scene into it and composite it back to the
--   window.  This is the common infrastructure for supersampling (SSAA),
--   region caching and any future post-processing.
--
--   Compatibility: the window composite prefers @glBlitFramebuffer@
--   (GL 3.0 / ARB_framebuffer_object, zero-copy GPU scaling); when that
--   is not available it falls back to a full-screen textured quad with
--   linear filtering (GL 2.1).  The support check is done once and cached.
module Graphics.Gloss.Internals.Rendering.Framebuffer
        ( ssaaFactor
        , RT(..)
        , newRT, deleteRT, getRT
        , withRenderTarget
        , blitToWindow
        , readPixelsRT
        , framebufferBlitSupported
        ) where

import qualified Graphics.Rendering.OpenGL.GL          as GL
import Graphics.Rendering.OpenGL                       (($=))
import qualified Graphics.Rendering.OpenGL.GL.ReadCopyPixels as ReadCopy
import Control.Monad (when)
import Data.IORef
import System.IO.Unsafe
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Ptr (nullPtr)

-- | Supersampling factor: the scene is rendered offscreen at this
--   multiple of the window resolution and composited down with linear
--   filtering.  Set to 1.0 to disable supersampling (still goes through
--   the offscreen pipeline at 1:1, which is behaviourally identical to a
--   direct render).
ssaaFactor :: Float
ssaaFactor = 1.2

-- | Offscreen render target.
data RT = RT
    { rtFBO :: GL.FramebufferObject
    , rtTex :: GL.TextureObject
    , rtDim :: (Int, Int)  -- ^ pixel size of the texture
    }

-- | Create a new render target with the given pixel size.
newRT :: (Int, Int) -> IO RT
newRT (pw, ph) = do
    fbo <- GL.genObjectName
    GL.bindFramebuffer GL.Framebuffer $= fbo

    tex <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just tex
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8
        (GL.TextureSize2D (fromIntegral pw) (fromIntegral ph)) 0
        (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
    -- linear filtering for the downsampling composite
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

    GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0
    GL.drawBuffer $= GL.FBOColorAttachment 0
    GL.readBuffer $= GL.FBOColorAttachment 0

    status <- GL.get (GL.framebufferStatus GL.Framebuffer)
    when (status /= GL.Complete)
        $ error $ "Framebuffer FBO incomplete: " ++ show status

    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    return $ RT fbo tex (pw, ph)

-- | Delete a render target (unbind first).
deleteRT :: RT -> IO ()
deleteRT rt = do
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    GL.deleteObjectName (rtFBO rt)
    GL.deleteObjectName (rtTex rt)

-- | Get a render target from a pool for the given window size, creating
--   or resizing it as needed.  The pool is an 'IORef' holding @Nothing@
--   initially (created lazily on first use).
getRT :: IORef (Maybe RT) -> (Int, Int) -> IO RT
getRT pool win@(w, h) = do
    let (pw, ph) = pixelDim win
    m <- readIORef pool
    case m of
      Just rt | rtDim rt == (pw, ph) -> return rt
      _ -> do
        mapM_ deleteRT m
        rt <- newRT (pw, ph)
        writeIORef pool (Just rt)
        return rt

-- | Pixel dimensions for a logical window size under supersampling.
pixelDim :: (Int, Int) -> (Int, Int)
pixelDim (w, h) =
    ( max 1 (ceiling (fromIntegral w * ssaaFactor))
    , max 1 (ceiling (fromIntegral h * ssaaFactor)) )

-- | Render @action@ into the render target.
--
--   Binds the FBO, sets the viewport to the target's pixel size and the
--   orthographic projection to the LOGICAL window size (so picture
--   coordinates are unaffected by supersampling), runs @action@, then
--   unbinds back to the default framebuffer.  The previous viewport and
--   framebuffer are restored.
withRenderTarget :: RT -> (Int, Int) -> IO () -> IO ()
withRenderTarget rt logicalSize action = do
    oldVP <- GL.get GL.viewport
    oldFB <- GL.get (GL.bindFramebuffer GL.Framebuffer)
    let (pw, ph) = rtDim rt
        (w, h)  = logicalSize
    GL.bindFramebuffer GL.Framebuffer $= rtFBO rt
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral pw) (fromIntegral ph))
    GL.matrixMode $= GL.Projection
    GL.preservingMatrix $ do
        GL.loadIdentity
        let (sx, sy) = (fromIntegral w / 2, fromIntegral h / 2)
        GL.ortho (-sx) sx (-sy) sy 0 (-100)
        GL.matrixMode $= GL.Modelview 0
        action
        GL.matrixMode $= GL.Projection
    GL.bindFramebuffer GL.Framebuffer $= oldFB
    GL.viewport $= oldVP
    GL.matrixMode $= GL.Modelview 0

-- | Composite the render target back to the default (window) framebuffer,
--   scaling it to the logical window size with linear filtering.
--
--   Uses @glBlitFramebuffer@ when supported, otherwise a full-screen
--   textured quad.
blitToWindow :: RT -> (Int, Int) -> IO ()
blitToWindow rt logicalSize = do
    let (pw, ph) = rtDim rt
        (w, h)  = logicalSize
        (sx, sy) = (fromIntegral w / 2, fromIntegral h / 2)
    supported <- framebufferBlitSupported
    if supported
        then do
            -- zero-copy GPU downsample
            GL.bindFramebuffer GL.ReadFramebuffer  $= rtFBO rt
            GL.bindFramebuffer GL.DrawFramebuffer $= GL.defaultFramebufferObject
            ReadCopy.blitFramebuffer
                (GL.Position 0 0) (GL.Position (fromIntegral pw) (fromIntegral ph))
                (GL.Position 0 0) (GL.Position (fromIntegral w) (fromIntegral h))
                [ReadCopy.ColorBuffer'] GL.Linear'
            GL.bindFramebuffer GL.ReadFramebuffer  $= GL.defaultFramebufferObject
            GL.bindFramebuffer GL.DrawFramebuffer $= GL.defaultFramebufferObject
        else
            drawQuad rt (w, h) (sx, sy)

-- | GL 2.1 fallback: full-screen textured quad sampling the RT texture.
--   The scene was rendered bottom-up into the texture; the quad's
--   texcoord y axis is flipped so the image is not mirrored.
drawQuad :: RT -> (Int, Int) -> (Float, Float) -> IO ()
drawQuad rt (w, h) (sx, sy) = do
    let (pw, ph) = rtDim rt
        (pwF, phF) = (fromIntegral pw :: Float, fromIntegral ph :: Float)
    GL.matrixMode $= GL.Projection
    GL.preservingMatrix $ do
        GL.loadIdentity
        GL.ortho (realToFrac (-sx)) (realToFrac sx) (realToFrac (-sy)) (realToFrac sy) 0 (-100)
        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity
        -- enable texturing (required: without it the quad is drawn in the
        -- current color, which is black after the clear)
        GL.texture GL.Texture2D $= GL.Enabled
        GL.textureFunction $= GL.Replace
        oldColor <- GL.get GL.currentColor
        GL.currentColor $= GL.Color4 1 1 1 1
        GL.textureBinding GL.Texture2D $= Just (rtTex rt)
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
        GL.renderPrimitive GL.Quads $ do
            let tx a b = GL.texCoord (GL.TexCoord2 (realToFrac a :: GL.GLfloat) (realToFrac b :: GL.GLfloat))
                vx a b  = GL.vertex (GL.Vertex2 a b)
            tx 0 0 >> vx (-sx) (-sy)
            tx pwF 0 >> vx sx (-sy)
            tx pwF phF >> vx sx sy
            tx 0 phF >> vx (-sx) sy
        GL.textureBinding GL.Texture2D $= Nothing
        GL.currentColor $= oldColor
        GL.texture GL.Texture2D $= GL.Disabled
        GL.matrixMode $= GL.Projection
    GL.matrixMode $= GL.Modelview 0

-- | Read back the RT's pixels to the CPU (RGBA, bottom-up).  Used for
--   hit-testing (e.g. region alpha lookup); not needed for display.
readPixelsRT :: RT -> IO (ForeignPtr Word8)
readPixelsRT rt = do
    let (pw, ph) = rtDim rt
    fptr <- mallocForeignPtrBytes (pw * ph * 4)
    GL.bindFramebuffer GL.Framebuffer $= rtFBO rt
    withForeignPtr fptr $ \ptr ->
        GL.readPixels (GL.Position 0 0) (GL.Size (fromIntegral pw) (fromIntegral ph))
            (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    return fptr

-- | Whether @glBlitFramebuffer@ is available (GL >= 3.0 or
--   ARB_framebuffer_object).  Checked once, cached.
framebufferBlitSupported :: IO Bool
framebufferBlitSupported = readIORef blitSupportedRef

blitSupportedRef :: IORef Bool
blitSupportedRef = unsafePerformIO $ do
    ref <- newIORef False
    supported <- queryBlitSupport
    writeIORef ref supported
    return ref
{-# NOINLINE blitSupportedRef #-}

queryBlitSupport :: IO Bool
queryBlitSupport = do
    ver <- GL.get GL.glVersion
    -- GL version string like "4.6.0 NVIDIA ..." or "3.3.0 ..." or "2.1 ..."
    let major = case reads (takeWhile isDigit ver) of
            [(m, _)] -> (m :: Int)
            _        -> 0
    return $ major >= 3
  where
    isDigit c = c >= '0' && c <= '9'

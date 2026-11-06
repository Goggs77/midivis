module Graphics.Gloss.Relative.Internal.Raster where

import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Gloss.Rendering as Gloss
import Graphics.Gloss
import Data.ByteString.Internal (create)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Foreign
import Foreign.ForeignPtr
import Data.Word
import Data.IORef
import Control.Monad
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, makeStableName)


data Offscreen = Offscreen
    { offFBO  :: GL.FramebufferObject
    , offTex  :: GL.TextureObject
    , offDim  :: ScreenSize     -- ^ logical (window) size
    }

createOffscreen :: ScreenSize -> IO Offscreen
createOffscreen dim@(w,h) = do
    -- texture is supersampled; logical size kept for resize checks
    let (pw, ph) = ssaaDim (w,h)
    fbo <- GL.genObjectName
    GL.bindFramebuffer GL.Framebuffer $= fbo
    
    tex <- GL.genObjectName
    GL.textureBinding GL.Texture2D $= Just tex
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (fromIntegral pw) (fromIntegral ph)) 0 (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
    -- CRITICAL: linear filtering, no mipmaps.  The default min filter is
    -- NEAREST_MIPMAP_LINEAR which requires a complete mipmap chain; with
    -- only level 0 the texture is incomplete and ANY sampling (e.g. the
    -- FX shader quad) returns black.
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
    
    GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0
    
    GL.drawBuffer $= GL.FBOColorAttachment 0
    GL.readBuffer $= GL.FBOColorAttachment 0
    
    status <- GL.get (GL.framebufferStatus GL.Framebuffer)
    when (status /= GL.Complete) $ error $ "Offscreen FBO incomplete " ++ show status
    
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    return Offscreen { offFBO = fbo, offTex = tex, offDim = dim }

-- | Pixel dimensions for a logical size under supersampling.
ssaaDim :: ScreenSize -> ScreenSize
ssaaDim (w,h) = (ceiling (fromIntegral w * ssaaFactor)
                , ceiling (fromIntegral h * ssaaFactor))

deleteOffscreen :: Offscreen -> IO ()
deleteOffscreen off = do
  -- Unbind first
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.textureBinding GL.Texture2D $= Nothing

  -- Delete GPU objects
  GL.deleteObjectName (offFBO off)
  GL.deleteObjectName (offTex off)

-- * Temporal anti-aliasing (TAA)
--
--   A light-weight temporal AA over the FX pipeline: every frame the scene
--   is rendered with a tiny sub-pixel jitter, then the shader's output is
--   blended with the previous frame's resolved texture.  Over a few frames
--   this accumulates samples at different sub-pixel positions, smoothing
--   edges without extra supersampling passes.

-- | History of the previous frame's resolved texture.  Keyed by logical
--   size so a resize discards stale history instead of sampling a texture
--   whose dimensions no longer match.
data TAAHist = TAAHist
    { thTex :: GL.TextureObject
    , thDim :: ScreenSize
    }

-- | Previous frame's resolved texture for each active shader program.
--   Keeping one history per shader prevents nested FX passes from consuming
--   or overwriting each other's temporal history.
taaHistRef :: IORef [(StableName Gloss.Shader, TAAHist)]
taaHistRef = unsafePerformIO (newIORef [])
{-# NOINLINE taaHistRef #-}

-- | Monotonic frame counter used to pick the sub-pixel jitter sample.
taaFrameRef :: IORef Int
taaFrameRef = unsafePerformIO (newIORef 0)
{-# NOINLINE taaFrameRef #-}

-- | Sub-pixel jitter offset for the current frame, in pixels, each axis in
--   @[-0.5, 0.5)@.  Uses the first eight Halton(2,3) points so successive
--   frames sample different sub-pixel positions.  An 8-frame cycle makes
--   the residual shimmer noise-like instead of a slow, visible 4-frame beat.
jitterOffset :: Int -> (Float, Float)
jitterOffset frame =
    let haltonX = [0.5, 0.25, 0.75, 0.125, 0.625, 0.375, 0.875, 0.0625] :: [Float]
        haltonY = [0.33333334, 0.6666667, 0.11111111, 0.44444445, 0.7777778, 0.22222222, 0.5555556, 0.8888889] :: [Float]
        n = frame `mod` 8
    in (haltonX !! n - 0.5, haltonY !! n - 0.5)

-- | TAA resolve shader: blends the current frame (@uTex@, unit 0) with the
--   history frame (@uHist@, unit 1).
--
--   Standard TAA resolve: the history colour is clamped to the current
--   frame's 3x3-neighbourhood AABB before blending.  This "neighbourhood
--   clamp" removes ghosting (history colours that ran off to a stale
--   position get pulled back), which is what lets the history weight stay
--   high to suppress shimmer.  The small current-frame weight (0.15) then
--   smooths edges without either artefact dominating.
taaVertSrc :: String
taaVertSrc = unlines
    [ "#version 120"
    , "attribute vec2 aPos;"
    , "attribute vec2 aUV;"
    , "varying vec2 vUV;"
    , "void main() { gl_Position = vec4(aPos, 0.0, 1.0); vUV = aUV; }"
    ]

taaFragSrc :: String
taaFragSrc = unlines
    [ "#version 120"
    , "uniform sampler2D uTex;"
    , "uniform sampler2D uHist;"
    , "uniform vec2 uTexSize;"
    , "varying vec2 vUV;"
    , "void main() {"
    , "  vec2 uv    = vUV / uTexSize;"
    , "  vec2 texel = 1.0 / uTexSize;"
    , "  vec3 cur = texture2D(uTex, uv).rgb;"
    , "  // 3x3 neighbourhood of the current frame"
    , "  vec3 s00 = texture2D(uTex, uv + texel*vec2(-1.0,-1.0)).rgb;"
    , "  vec3 s10 = texture2D(uTex, uv + texel*vec2( 0.0,-1.0)).rgb;"
    , "  vec3 s20 = texture2D(uTex, uv + texel*vec2( 1.0,-1.0)).rgb;"
    , "  vec3 s01 = texture2D(uTex, uv + texel*vec2(-1.0, 0.0)).rgb;"
    , "  vec3 s21 = texture2D(uTex, uv + texel*vec2( 1.0, 0.0)).rgb;"
    , "  vec3 s02 = texture2D(uTex, uv + texel*vec2(-1.0, 1.0)).rgb;"
    , "  vec3 s12 = texture2D(uTex, uv + texel*vec2( 0.0, 1.0)).rgb;"
    , "  vec3 s22 = texture2D(uTex, uv + texel*vec2( 1.0, 1.0)).rgb;"
    , "  // AABB over the neighbourhood (min/max per colour channel)"
    , "  vec3 mn = min(min(min(s00,s10),min(s20,s01)),min(min(s21,s02),min(s12,s22)));"
    , "  vec3 mx = max(max(max(s00,s10),max(s20,s01)),max(max(s21,s02),max(s12,s22)));"
    , "  // include the centre pixel so current is never clamped away"
    , "  mn = min(mn, cur); mx = max(mx, cur);"
    , "  vec3 hist = clamp(texture2D(uHist, uv).rgb, mn, mx);"
    , "  gl_FragColor = vec4(mix(hist, cur, 0.35), 1.0);"
    , "}"
    ]

-- | Lazily-compiled TAA resolve program.  A top-level CAF: compiled once on
--   first use (inside the draw callback, so a GL context exists), then
--   cached by the CAF's own lazy evaluation.
shaderTAA :: Gloss.Shader
shaderTAA = unsafePerformIO (Gloss.compileShader taaVertSrc taaFragSrc)
{-# NOINLINE shaderTAA #-}

-- | Delete an offscreen's FBO but keep its texture.  Used when a texture is
--   promoted to the TAA history buffer and must outlive its FBO.
deleteFBOKeepTex :: Offscreen -> IO ()
deleteFBOKeepTex off = do
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    GL.deleteObjectName (offFBO off)

getResizeOffscreen :: IORef (IO Offscreen) -> ScreenSize -> IO Offscreen
getResizeOffscreen r (w,h) = do
    moff <- readIORef r
    off <- moff
    let (offw,offh) = offDim off
    -- Recreate on ANY size change (grow OR shrink).  The old code only
    -- rebuilt when the window grew, so after shrinking the offscreen
    -- texture stayed at the old large size: rasterPictureFX then wrote
    -- into its bottom-left corner and the resulting bitmap rendered at
    -- the old size — spilling outside the smaller window.
    off' <- if w /= offw || h /= offh
        then do
            deleteOffscreen off
            off' <- createOffscreen (w,h)
            return off'
        else return off
    writeIORef r $! return $! off'
    return off'
    
rasterPicture
    :: Gloss.State -> Offscreen
    -> ScreenSize
    -> Picture
    -> IO RasteredPicture
rasterPicture state offscreen dim@(w,h) pic = do
    -- Save OpenGL state
    oldVP   <- GL.get GL.viewport
    
    -- supersample: render at a higher resolution, display at logical size
    let (rw, rh) = ssaaDim (w,h)
    
    -- Bind offscreen target
    GL.bindFramebuffer GL.Framebuffer $= (offFBO offscreen)
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral rw) (fromIntegral rh))
    
    -- Clear if needed (alpha = 0 for transparency)
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    
    -- Set projection to match Gloss expectations (logical coords)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho (-fromIntegral w / 2) ( fromIntegral w / 2) (-fromIntegral h / 2) ( fromIntegral h / 2) (-1) 1
    
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    
    -- Render Gloss picture
    renderPicture state 1 pic
    
    -- Read pixels (RGBA, bottom-up) at the supersampled resolution
    let bytes = rw * rh * 4
    fptr <- mallocForeignPtrBytes bytes
    withForeignPtr fptr $ \ptr ->
        GL.readPixels (GL.Position 0 0) (GL.Size (fromIntegral rw) (fromIntegral rh)) (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    
    -- Restore GL state
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    GL.viewport $= oldVP
    
    -- Convert to Picture (logical dim, supersampled pixels)
    return (dim,(rw,rh),BitmapFormat BottomToTop PxRGBA,fptr)

-- | Like 'rasterPicture', but runs a GLSL post-process shader over the
--   offscreen render and returns a picture backed by the resolved GPU
--   texture.
--
--   Pipeline: render the scene into @offscreen@ (FBO A), then bind a
--   second output FBO (B) and draw a full-screen textured quad sampling A
--   with the given shader.  TAA resolves into C and C's texture becomes
--   both the history and the returned picture.  No CPU readback or texture
--   re-upload is needed.
rasterPictureFX
    :: Gloss.State -> Offscreen -> Gloss.Shader
    -> (Gloss.Shader -> IO ())   -- ^ per-pass uniform setup (program bound)
    -> ScreenSize -> Picture
    -> IO Picture
rasterPictureFX state offscreen sh setup dim@(w,h) pic = do
    oldVP <- GL.get GL.viewport
    -- Render at ssaaDim (1.2x): the shared offscreen pool allocates its
    -- textures at ssaaDim, so the viewport MUST match that size or the
    -- scene lands in the bottom-left corner of a too-big texture and
    -- sampling picks up only a fraction of it.
    let (rw, rh) = ssaaDim (w,h)
    frame <- atomicModifyIORef' taaFrameRef (\n -> (n + 1, n))
    -- Sub-pixel jitter (TAA): shift the projection by a fraction of a
    -- pixel each frame so successive frames sample different sub-pixel
    -- positions.  Convert pixels -> logical coords (rw px spans w units).
    let (jpx, jpy) = jitterOffset frame
        ox = realToFrac jpx * fromIntegral w / fromIntegral rw
        oy = realToFrac jpy * fromIntegral h / fromIntegral rh

    -- 1. render the scene into FBO A (with jitter)
    GL.bindFramebuffer GL.Framebuffer $= offFBO offscreen
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral rw) (fromIntegral rh))
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho (-fromIntegral w / 2 + ox) (fromIntegral w / 2 + ox)
             (-fromIntegral h / 2 + oy) (fromIntegral h / 2 + oy) (-1) 1
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    renderPicture state 1 pic

    -- 2. shader pass into a temporary output FBO B
    outOff <- createOffscreen (w,h)
    GL.bindFramebuffer GL.Framebuffer $= offFBO outOff
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral rw) (fromIntegral rh))
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    -- sample FBO A's texture (unit 0), post-processed into B.
    -- Blend must be disabled for the whole post-process chain (shader pass
    -- AND TAA resolve): the scene has low-alpha pixels and these passes
    -- overwrite rather than blend.  Restored after step 3.
    oldBlend <- GL.get $ GL.blend
    GL.blend $= GL.Disabled
    GL.textureBinding GL.Texture2D $= Just (offTex offscreen)
    GL.texture GL.Texture2D $= GL.Enabled
    -- uTexSize must be set AFTER the program is bound (inside the quad
    -- draw's setup callback), otherwise GL.uniform is a silent no-op.
    -- The caller's per-pass setup (e.g. uniform arrays) runs first so it can
    -- override nothing / coexist with uTexSize.
    Gloss.renderTexturedQuadWithShader sh (rw, rh) $ do
        setup sh
        Gloss.uniform2f sh "uTexSize" (fromIntegral rw, fromIntegral rh)
    GL.texture GL.Texture2D $= GL.Disabled
    GL.textureBinding GL.Texture2D $= Nothing

    -- 3. TAA resolve: blend B (current) with the previous frame's history
    --    texture into a third FBO C, then read C back.  On the first frame
    --    (or after a resize) there is no history, so B is used directly and
    --    B's texture seeds the history.
    shName <- makeStableName sh
    histories <- readIORef taaHistRef
    let mHist = lookup shName histories
        storeHistory hist =
            writeIORef taaHistRef $
                (shName, hist) : filter ((/= shName) . fst) histories
    outTex <- case mHist of
        Just hist | thDim hist == (w,h) -> do
            cOff <- createOffscreen (w,h)
            GL.bindFramebuffer GL.Framebuffer $= offFBO cOff
            GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral rw) (fromIntegral rh))
            GL.clearColor $= GL.Color4 0 0 0 0
            GL.clear [GL.ColorBuffer]
            -- current frame on unit 0, history on unit 1
            GL.activeTexture $= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D $= Just (offTex outOff)
            GL.activeTexture $= GL.TextureUnit 1
            GL.textureBinding GL.Texture2D $= Just (thTex hist)
            Gloss.renderTexturedQuadWithShader shaderTAA (rw, rh) $ do
                Gloss.uniform1i shaderTAA "uTex" 0
                Gloss.uniform1i shaderTAA "uHist" 1
                Gloss.uniform2f shaderTAA "uTexSize" (fromIntegral rw, fromIntegral rh)
            -- unbind unit 1, back to unit 0, unbind unit 0
            GL.activeTexture $= GL.TextureUnit 1
            GL.textureBinding GL.Texture2D $= Nothing
            GL.activeTexture $= GL.TextureUnit 0
            GL.textureBinding GL.Texture2D $= Nothing
            -- C's texture becomes the new history; drop the old history
            -- texture and B entirely.
            GL.deleteObjectName (thTex hist)
            deleteOffscreen outOff
            storeHistory (TAAHist (offTex cOff) (w,h))
            deleteFBOKeepTex cOff
            return (offTex cOff)
        _ -> do
            -- First frame / resize: B's texture is both output and history.
            case mHist of
                Just h  -> GL.deleteObjectName (thTex h)
                Nothing -> return ()
            storeHistory (TAAHist (offTex outOff) (w,h))
            deleteFBOKeepTex outOff
            return (offTex outOff)
    GL.blend $= oldBlend

    -- 4. restore state
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
    GL.viewport $= oldVP
    -- The returned picture borrows the resolved texture.  'taaHistRef' owns
    -- it, and keeps it alive until the next rendered frame or resize.
    return $ Scale s s $
        BitmapSection (Rectangle (0,0) (rw,rh)) $
            Gloss.bitmapDataOfTexture rw rh
                (BitmapFormat BottomToTop PxRGBA)
                (case outTex of GL.TextureObject textureId -> fromIntegral textureId)
  where
    s = fromIntegral w / fromIntegral (fst $ ssaaDim (w,h))

-- | A rasterised picture: logical display size, actual pixel size,
--   pixel format and raw RGBA bytes (bottom-up).  The pixel size may
--   differ from the logical size when supersampling is active.
type RasteredPicture = (ScreenSize, ScreenSize, BitmapFormat, ForeignPtr Word8)

fromRasteredPicture :: RasteredPicture -> Picture
fromRasteredPicture ((w,h),(pw,ph),fmt,fptr) =
    -- The texture is ssaaDim (1.2x) pixels; BitmapSection's texture
    -- coords are section/bitmapSize, so the section must span the FULL
    -- pixel size to sample the whole texture, and a UNIFORM scale brings
    -- the display back to the logical size.  Using one factor for both
    -- axes keeps the aspect ratio exact (the naive w/pw vs h/ph pair
    -- differs by a ceiling rounding and stretches circles into ellipses).
    -- cacheMe = False so per-frame ForeignPtrs don't accumulate in
    -- gloss's StableName-keyed texture cache (a GB/s leak).
    Scale s s $
        BitmapSection
            (Rectangle (0,0) (pw,ph))
            (bitmapDataOfForeignPtr pw ph fmt fptr False)
  where
    s = fromIntegral w / fromIntegral pw

-- | Checks if the pixel of a certain point (with (0,0) centered in the image) is transparent.
getPixelAlpha :: RasteredPicture -> Point -> IO Word8
getPixelAlpha pic@(dim,_,_,_) p = do
    -- scale logical point into the (possibly supersampled) pixel grid
    alpha <- lookupAlpha pic (centerToTopLeft' pic p)
    return $ alpha

-- | Map a centered logical point to a top-left pixel coordinate in the
--   actual pixel grid (which may be supersampled relative to the dim).
centerToTopLeft' :: RasteredPicture -> Point -> (Int,Int)
centerToTopLeft' ((w,h),(pw,ph),_,_) (xc, yc) =
  ( round $ (xc + realToFrac w / 2) * realToFrac pw / realToFrac w
  , round $ (realToFrac h / 2 - yc) * realToFrac ph / realToFrac h
  )

topLeftToCenter :: ScreenSize -> (Int,Int) -> Point
topLeftToCenter (w,h) (xt, yt) =
  ( realToFrac xt - realToFrac w / 2
  , realToFrac h / 2 - realToFrac yt
  )

-- coordinates (x,y) with (0,0) being the top-left corner.
lookupAlpha :: RasteredPicture -> (Int,Int) -> IO Word8
lookupAlpha pic@((w,h),(pw,ph),fmt,fptr) (x,y) =
  withForeignPtr fptr $ \ptr -> do
    let off = pixelOffset pic x y + alphaOffset (pixelFormat fmt)
    peekByteOff ptr off

alphaOffset :: PixelFormat -> Int
alphaOffset PxRGBA = 3
alphaOffset PxABGR = 0

pixelStride :: Int
pixelStride = 4

pixelOffset :: RasteredPicture -> Int -> Int -> Int
pixelOffset ((_,_),(pw,ph),fmt,_) x y =
  let BitmapFormat ro pf = fmt
      ry       = rowIndex ro ph y
      base     = (ry * pw + x) * pixelStride
  in base

rowIndex :: RowOrder -> Int -> Int -> Int
rowIndex TopToBottom h y = y
rowIndex BottomToTop h y = h - 1 - y


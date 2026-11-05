{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Rendering.Picture
        (renderPicture, lineStripBufferPicture)
where
import Graphics.Gloss.Internals.Rendering.State
import Graphics.Gloss.Internals.Rendering.Common
import Graphics.Gloss.Internals.Rendering.Circle
import Graphics.Gloss.Internals.Rendering.Polygon
import Graphics.Gloss.Internals.Rendering.Bitmap
import Graphics.Gloss.Internals.Rendering.Shader
import Graphics.Gloss.Internals.Data.Picture
import Graphics.Gloss.Internals.Data.Color
import System.Mem.StableName
import Foreign.ForeignPtr
import Foreign.Ptr (nullPtr)
import Foreign.Storable (sizeOf)
import Data.IORef
import Data.List
import Control.Monad
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.Rendering.OpenGL.GL           as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors   as GLU
import qualified Graphics.UI.GLUT                       as GLUT
import System.IO.Unsafe (unsafePerformIO)


-- | Render a picture into the current OpenGL context.
--
--   Assumes that the OpenGL matrix mode is set to @Modelview@
--
renderPicture
        :: State        -- ^ Current rendering state.
        -> Float        -- ^ View port scale, which controls the level of detail.
                        --   Use 1.0 to start with.
        -> Picture      -- ^ Picture to render.
        -> IO ()

renderPicture state circScale picture
 = do
        -- Setup render state for world
        setLineSmooth   (stateLineSmooth state)
        setBlendAlpha   (stateBlendAlpha state)

        -- Draw the picture
        checkErrors "before drawPicture."
        drawPicture state circScale picture
        checkErrors "after drawPicture."


drawPicture :: State -> Float -> Picture -> IO ()
drawPicture state circScale picture
 = {-# SCC "drawComponent" #-}
   case picture of

        -- nothin'
        Blank
         ->     return ()

        -- line
        Line path
         -> GL.renderPrimitive GL.LineStrip
                $ vertexPFs path

        -- polygon (where?)
        Polygon path
         | stateWireframe state
         -> GL.renderPrimitive GL.LineLoop
                $ vertexPFs path

         | otherwise
         -> renderComplexPolygon path

        -- circle
        Circle radius
         ->  renderCircle 0 0 circScale radius 0

        ThickCircle radius thickness
         ->  renderCircle 0 0 circScale radius thickness

        -- arc
        Arc a1 a2 radius
         ->  renderArc 0 0 circScale radius a1 a2 0

        ThickArc a1 a2 radius thickness
         ->  renderArc 0 0 circScale radius a1 a2 thickness

        -- stroke text
        --      text looks weird when we've got blend on,
        --      so disable it during the renderString call.
        Text str
         -> do
                GL.blend        $= GL.Disabled
                GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
                GL.blend        $= GL.Enabled

        -- colors with float components.
        Color col p
         |  stateColor state
         ->  do oldColor         <- get GL.currentColor

                let RGBA r g b a  = col

                GL.currentColor  $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
                drawPicture state circScale p
                GL.currentColor  $= oldColor

         |  otherwise
         ->     drawPicture state circScale p


        -- Translation --------------------------
        -- Easy translations are done directly to avoid calling GL.perserveMatrix.
        Translate posX posY (Circle radius)
         -> renderCircle posX posY circScale radius 0

        Translate posX posY (ThickCircle radius thickness)
         -> renderCircle posX posY circScale radius thickness

        Translate posX posY (Arc a1 a2 radius)
         -> renderArc posX posY circScale radius a1 a2 0

        Translate posX posY (ThickArc a1 a2 radius thickness)
         -> renderArc posX posY circScale radius a1 a2 thickness

        Translate tx ty (Rotate deg p)
         -> GL.preservingMatrix
          $ do  GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
                GL.rotate    (gf deg) (GL.Vector3 0 0 (-1))
                drawPicture state circScale p

        Translate tx ty p
         -> GL.preservingMatrix
          $ do  GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
                drawPicture state circScale p

        -- Rotation -----------------------------
        -- Easy rotations are done directly to avoid calling GL.perserveMatrix.
        Rotate _   (Circle radius)
         -> renderCircle   0 0 circScale radius 0

        Rotate _   (ThickCircle radius thickness)
         -> renderCircle   0 0 circScale radius thickness

        Rotate deg (Arc a1 a2 radius)
         -> renderArc      0 0 circScale radius (a1-deg) (a2-deg) 0

        Rotate deg (ThickArc a1 a2 radius thickness)
         -> renderArc      0 0 circScale radius (a1-deg) (a2-deg) thickness


        Rotate deg p
         -> GL.preservingMatrix
          $ do  GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
                drawPicture state circScale p

        -- Scale --------------------------------
        Scale sx sy p
         -> GL.preservingMatrix
          $ do  GL.scale (gf sx) (gf sy) 1
                let mscale      = max sx sy
                drawPicture state (circScale * mscale) p

        Bitmap imgData ->
          let (width, height) = bitmapSize imgData
          in
            drawPicture state circScale $
              BitmapSection (rectAtOrigin width height) imgData

        BitmapSection
          Rectangle
            { rectPos = imgSectionPos
            , rectSize = imgSectionSize }
          imgData@BitmapData
          { bitmapSize = (width, height)
          , bitmapCacheMe = cacheMe
          , bitmapTexture = mbTexture
          , bitmapTint = tint }
          ->
           do
            let rowInfo =
                  -- calculate texture coordinates
                  -- remark:
                  --   On some hardware, using exact "integer" coordinates causes texture coords
                  --   with a component == 0  flip to -1. This appears as the texture flickering
                  --   on the left and sometimes show one additional row of pixels outside the
                  --   given rectangle
                  --   To prevent this we add an "epsilon-border".
                  --   This has been testet to fix the problem.
                  let defTexCoords =
                        map (\(x,y) -> (x / fromIntegral width, y / fromIntegral height)) $
                        [ vecMap (+eps) (+eps) $ toFloatVec imgSectionPos
                        , vecMap (subtract eps) (+eps) $ toFloatVec $
                            ( fst imgSectionPos + fst imgSectionSize
                            , snd imgSectionPos )
                        , vecMap (subtract eps) (subtract eps) $ toFloatVec $
                            ( fst imgSectionPos + fst imgSectionSize
                            , snd imgSectionPos + snd imgSectionSize )
                        , vecMap (+eps) (subtract eps) $ toFloatVec $
                            ( fst imgSectionPos
                            , snd imgSectionPos + snd imgSectionSize )
                        ]
                        :: [(Float,Float)]
                      toFloatVec = vecMap fromIntegral fromIntegral
                      vecMap :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
                      vecMap f g (x,y) = (f x, g y)
                      eps = 0.001 :: Float
                  in
                    case rowOrder (bitmapFormat imgData) of
                      BottomToTop -> defTexCoords
                      TopToBottom -> reverse defTexCoords

            let drawWith texObj wrapping filtering = do
                  GL.textureWrapMode GL.Texture2D GL.S $= wrapping
                  GL.textureWrapMode GL.Texture2D GL.T $= wrapping
                  GL.textureFilter   GL.Texture2D      $= filtering
                  GL.texture GL.Texture2D $= GL.Enabled
                  GL.textureFunction      $= GL.Combine
                  GL.textureBinding GL.Texture2D $= Just texObj

                  oldColor <- get GL.currentColor
                  when (not tint) $
                    GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
                  GL.renderPrimitive GL.Polygon $
                    forM_ (bitmapPath (fromIntegral $ fst imgSectionSize)
                                      (fromIntegral $ snd imgSectionSize) `zip` rowInfo) $
                    \((polygonCoordX, polygonCoordY), (textureCoordX,textureCoordY)) ->
                    do
                      GL.texCoord $ GL.TexCoord2 (gf textureCoordX) (gf textureCoordY)
                      GL.vertex   $ GL.Vertex2   (gf polygonCoordX) (gf polygonCoordY)
                  GL.currentColor $= oldColor
                  GL.texture GL.Texture2D $= GL.Disabled

            case mbTexture of
              Just textureId ->
                drawWith
                  (GL.TextureObject (fromIntegral textureId))
                  (GL.Repeated, GL.ClampToEdge)
                  ((GL.Linear', Nothing), GL.Linear')

              Nothing -> do
                -- Load the image data into a texture,
                -- or grab it from the cache if we've already done that before.
                tex <- loadTexture (stateTextures state) imgData cacheMe
                drawWith
                  (texObject tex)
                  (GL.Repeated, GL.Repeat)
                  ((GL.Nearest, Nothing), GL.Nearest)

                -- Free uncachable texture objects.
                freeTexture tex

        LineStripBuffer bufferId vertexCount
         -> do
            -- Coordinates are transformed by Gloss's normal model-view
            -- matrix, so callers can place the strip in a scaled Frame.
            registered <- readIORef lineStripBuffersRef
            case lookup bufferId registered of
                Nothing -> error $ "Unknown GPU line buffer " ++ show bufferId
                Just buffer -> do
                    currentColor <- get GL.currentColor
                    oldBuffer <- get (GL.bindBuffer GL.ArrayBuffer)
                    oldAttrib <- get (GL.vertexAttribArray (GL.AttribLocation 0))
                    withShader lineStripShader $ do
                        case currentColor of
                            GL.Color4 r g b a ->
                                uniform4f lineStripShader "uColor" (r, g, b, a)
                        GL.bindBuffer GL.ArrayBuffer $= Just buffer
                        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
                        GL.vertexAttribPointer (GL.AttribLocation 0) $=
                            (GL.ToFloat, GL.VertexArrayDescriptor
                                2 GL.Float (fromIntegral (2 * sizeOf (0 :: Float))) nullPtr)
                        GL.drawArrays GL.LineStrip 0 (fromIntegral vertexCount)
                        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
                    GL.bindBuffer GL.ArrayBuffer $= oldBuffer
                    GL.vertexAttribArray (GL.AttribLocation 0) $= oldAttrib

        Pictures ps
         -> mapM_ (drawPicture state circScale) ps

lineStripShader :: Shader
lineStripShader = unsafePerformIO $ compileShader
    (unlines
        [ "#version 120"
        , "attribute vec2 aPos;"
        , "void main() {"
        , "  gl_Position = gl_ModelViewProjectionMatrix * vec4(aPos, 0.0, 1.0);"
        , "}"
        ])
    (unlines
        [ "#version 120"
        , "uniform vec4 uColor;"
        , "void main() { gl_FragColor = uColor; }"
        ])
{-# NOINLINE lineStripShader #-}

lineStripBuffersRef :: IORef [(Int, GL.BufferObject)]
lineStripBuffersRef = unsafePerformIO (newIORef [])
{-# NOINLINE lineStripBuffersRef #-}

lineStripBufferPicture :: GL.BufferObject -> Int -> IO Picture
lineStripBufferPicture buffer vertexCount = do
    registered <- readIORef lineStripBuffersRef
    case find (\(_, b) -> b == buffer) registered of
        Just (key, _) -> pure (LineStripBuffer key vertexCount)
        Nothing -> do
            let key = case registered of
                        [] -> 1
                        xs -> fst (maximum xs) + 1
            writeIORef lineStripBuffersRef ((key, buffer) : registered)
            pure (LineStripBuffer key vertexCount)

-- Errors ---------------------------------------------------------------------
checkErrors :: String -> IO ()
checkErrors place
 = do   errors          <- get $ GLU.errors
        when (not $ null errors)
         $ mapM_ (handleError place) errors

handleError :: String -> GLU.Error -> IO ()
handleError place err
 = case err of
    GLU.Error GLU.StackOverflow _
     -> error $ unlines
      [ "Gloss / OpenGL Stack Overflow " ++ show place
      , "  This program uses the Gloss vector graphics library, which tried to"
      , "  draw a picture using more nested transforms (Translate/Rotate/Scale)"
      , "  than your OpenGL implementation supports. The OpenGL spec requires"
      , "  all implementations to have a transform stack depth of at least 32,"
      , "  and Gloss tries not to push the stack when it doesn't have to, but"
      , "  that still wasn't enough."
      , ""
      , "  You should complain to your harware vendor that they don't provide"
      , "  a better way to handle this situation at the OpenGL API level."
      , ""
      , "  To make this program work you'll need to reduce the number of nested"
      , "  transforms used when defining the Picture given to Gloss. Sorry." ]

    -- Issue #32: Spurious "Invalid Operation" errors under Windows 7 64-bit.
    --   When using GLUT under Windows 7 it complains about InvalidOperation,
    --   but doesn't provide any other details. All the examples look ok, so
    --   we're just ignoring the error for now.
    GLU.Error GLU.InvalidOperation _
     -> return ()
    _
     -> error $ unlines
     [  "Gloss / OpenGL Internal Error " ++ show place
     ,  "  Please report this on haskell-gloss@googlegroups.com."
     ,  show err ]


-- Textures -------------------------------------------------------------------
-- | Load a texture into the OpenGL context, or retrieve the existing handle
--   from our own cache.
loadTexture
        :: IORef [Texture] -- ^ Existing texture cache.
        -> BitmapData      -- ^ Texture data.
        -> Bool            -- ^ Force cache for newly loaded textures.
        -> IO Texture

loadTexture refTextures imgData@BitmapData{ bitmapSize=(width,height) } cacheMe
 = do   textures        <- readIORef refTextures

        -- Try and find this same texture in the cache.
        name            <- makeStableName imgData
        let mTexCached
                = find (\tex -> texName   tex == name
                             && texWidth  tex == width
                             && texHeight tex == height)
                textures

        case mTexCached of
         Just tex
          ->    return tex

         Nothing
          -> do tex <- installTexture imgData
                when cacheMe
                 $ writeIORef refTextures (tex : textures)
                return tex


-- | Install a texture into the OpenGL context,
--   returning the new texture handle.
installTexture :: BitmapData -> IO Texture
installTexture bitmapData@(BitmapData _ fmt (width,height) cacheMe fptr _ _)
 = do
        let glFormat
                = case pixelFormat fmt of
                        PxABGR -> GL.ABGR
                        PxRGBA -> GL.RGBA

        -- Allocate texture handle for texture
        [tex] <- GL.genObjectNames 1
        GL.textureBinding GL.Texture2D $= Just tex

        -- Sets the texture in imgData as the current texture
        -- This copies the data from the pointer into OpenGL texture memory,
        -- so it's ok if the foreignptr gets garbage collected after this.
        withForeignPtr fptr
         $ \ptr ->
           GL.texImage2D
                GL.Texture2D
                GL.NoProxy
                0
                GL.RGBA8
                (GL.TextureSize2D
                        (gsizei width)
                        (gsizei height))
                0
                (GL.PixelData glFormat GL.UnsignedByte ptr)

        -- Make a stable name that we can use to identify this data again.
        -- If the user gives us the same texture data at the same size then we
        -- can avoid loading it into texture memory again.
        name    <- makeStableName bitmapData

        return  Texture
                { texName       = name
                , texWidth      = width
                , texHeight     = height
                , texData       = fptr
                , texObject     = tex
                , texCacheMe    = cacheMe }


-- | If this texture does not have its `cacheMe` flag set then delete it from
--   OpenGL and free the GPU memory.
freeTexture :: Texture -> IO ()
freeTexture tex
 | texCacheMe tex       = return ()
 | otherwise            = GL.deleteObjectNames [texObject tex]


-- Utils ----------------------------------------------------------------------
-- | Turn alpha blending on or off
setBlendAlpha :: Bool -> IO ()
setBlendAlpha state
        | state
        = do    GL.blend        $= GL.Enabled
                GL.blendFunc    $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

        | otherwise
        = do    GL.blend        $= GL.Disabled
                GL.blendFunc    $= (GL.One, GL.Zero)


-- | Turn line smoothing on or off
setLineSmooth :: Bool -> IO ()
setLineSmooth state
        | state         = GL.lineSmooth $= GL.Enabled
        | otherwise     = GL.lineSmooth $= GL.Disabled


vertexPFs ::    [(Float, Float)] -> IO ()
vertexPFs []    = return ()
vertexPFs ((x, y) : rest)
 = do   GL.vertex $ GL.Vertex2 (gf x) (gf y)
        vertexPFs rest
{-# INLINE vertexPFs #-}


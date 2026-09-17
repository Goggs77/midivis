{-# OPTIONS_HADDOCK hide #-}

-- | GLSL shader support for the gloss rendering pipeline.
--
--   Compiles a vertex + fragment program from source at load time (the
--   sources are embedded at compile time by the caller, so the program
--   stays static — no runtime file access).  A 'Shader' is compiled once
--   and cached; 'renderTexturedQuadWithShader' draws a full-screen quad
--   sampling whatever texture is currently bound, which is the primitive
--   used for post-processing passes over a framebuffer render target.
--
--   Compatible with fixed-function GL 2.1 contexts: uses vertex attribute
--   arrays (no VAOs) and GLSL 120 shaders.
module Graphics.Gloss.Internals.Rendering.Shader
        ( Shader
        , shaderProgramOf
        , compileShader
        , withShader
        , deleteShader
        , uniform1f, uniform1i, uniform2f, uniform1fv
        , renderTexturedQuadWithShader
        ) where

import qualified Graphics.Rendering.OpenGL.GL as GL
import Control.Monad (unless)
import Data.StateVar (($=), get)
import Data.ByteString.Char8 (pack)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr)

-- | A compiled and linked GLSL program.
newtype Shader = Shader { shaderProgram :: GL.Program }

-- | The underlying GL program object (for uniform queries etc.).
shaderProgramOf :: Shader -> GL.Program
shaderProgramOf = shaderProgram

-- | Compile a vertex and fragment shader from source and link them into
--   a program.  Throws an IO error with the driver log on failure.
--   Bind attribute locations for the standard post-processing quad
--   ('aPos', 'aUV') before linking.
compileShader :: String -> String -> IO Shader
compileShader vertSrc fragSrc = do
    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= pack vertSrc
    GL.compileShader vs
    okV <- get $ GL.compileStatus vs
    unless okV $ do
        logV <- get $ GL.shaderInfoLog vs
        GL.deleteObjectName vs
        error $ "Vertex shader compile failed:\n" ++ logV

    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fs $= pack fragSrc
    GL.compileShader fs
    okF <- get $ GL.compileStatus fs
    unless okF $ do
        logF <- get $ GL.shaderInfoLog fs
        GL.deleteObjectName fs
        error $ "Fragment shader compile failed:\n" ++ logF

    prog <- GL.createProgram
{- 
// in pass.vert
attribute vec2 aPos;
attribute vec2 aUV; 
varying vec2 vUV
-}
    GL.attribLocation prog "aPos" $= GL.AttribLocation 0
    GL.attribLocation prog "aUV"  $= GL.AttribLocation 1
    GL.attachShader prog vs
    GL.attachShader prog fs
    GL.linkProgram prog
    okL <- get $ GL.linkStatus prog
    GL.deleteObjectName vs
    GL.deleteObjectName fs
    unless okL $ do
        logL <- get $ GL.programInfoLog prog
        GL.deleteObjectName prog
        error $ "Program link failed:\n" ++ logL
    return $ Shader prog

-- | Use the shader for the duration of an action, then restore the
--   previous program.
withShader :: Shader -> IO a -> IO a
withShader sh action = do
    old <- get $ GL.currentProgram
    GL.currentProgram $= Just (shaderProgram sh)
    x <- action
    GL.currentProgram $= old
    return x

-- | Delete a shader program (frees GPU resources).
deleteShader :: Shader -> IO ()
deleteShader sh = GL.deleteObjectName (shaderProgram sh)

-- | Set a float uniform by name (no-op if absent).
uniform1f :: Shader -> String -> Float -> IO ()
uniform1f sh name val =
    withLocation sh name $ \loc -> GL.uniform loc $= (val :: GL.GLfloat)

-- | Set an int uniform by name (no-op if absent).
uniform1i :: Shader -> String -> Int -> IO ()
uniform1i sh name val =
    withLocation sh name $ \loc -> GL.uniform loc $= (fromIntegral val :: GL.GLint)

-- | Set a vec2 uniform by name (no-op if absent).
uniform2f :: Shader -> String -> (Float, Float) -> IO ()
uniform2f sh name (x, y) =
    withLocation sh name $ \loc ->
        GL.uniform loc $= GL.Vertex2 (x :: GL.GLfloat) (y :: GL.GLfloat)

-- | Set a float ARRAY uniform by name (no-op if absent).  The GLSL side
--   declares a fixed-size array, e.g. @uniform float uData[129];@.  The
--   OpenGL package exposes no glUniform1fv instance, so each element is set
--   individually via its @name[i]@ location (GLSL array elements are
--   individually addressable); 129 glUniform1f calls per frame is negligible.
uniform1fv :: Shader -> String -> [Float] -> IO ()
uniform1fv sh name vals =
    mapM_ (\ (i, v) -> uniform1f sh (name ++ "[" ++ show i ++ "]") v)
          (zip [0 :: Int ..] vals)

withLocation :: Shader -> String -> (GL.UniformLocation -> IO ()) -> IO ()
withLocation sh name f = do
    loc <- get $ GL.uniformLocation (shaderProgram sh) name
    if loc == GL.UniformLocation (-1) then return () else f loc

-- | Draw a full-screen quad textured with the currently bound 2D
--   texture, using the given shader.  The quad spans the whole clip
--   space; texture coordinates go from (0,0) to (texW,texH) so the
--   fragment shader can address texels directly with
--   @texture2D(uTex, vUV / uTexSize)@.
--
--   The @setup@ action runs after the program is bound but before the
--   draw; use it to set uniforms (they require the program to be
--   current, so this is the only place they take effect).
--
--   Vertex data is provided through attribute arrays at fixed locations
--   (aPos = 0, aUV = 1), compatible with GL 2.1.
renderTexturedQuadWithShader :: Shader -> (Int, Int) -> IO () -> IO ()
renderTexturedQuadWithShader sh (texW, texH) setup =
    withShader sh $ do
        setup
        -- full-screen quad in clip space + uv in pixels
        let w = fromIntegral texW :: Float
            h = fromIntegral texH :: Float
            verts :: [Float]
            verts = [ -1, -1,  0, 0
                    ,  1, -1,  w, 0
                    ,  1,  1,  w, h
                    , -1,  1,  0, h
                    , -1, -1,  0, 0
                    ,  1,  1,  w, h ]
        -- enable the two fixed attribute locations
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
        withArray verts $ \ptr -> do
            -- stride = 4 floats (pos2 + uv2); the array is [pos, uv] pairs
            let stride = 4 * 4  -- bytes
                desc n off = GL.VertexArrayDescriptor
                    n
                    GL.Float
                    (fromIntegral stride)
                    (ptr `plusPtr` off)
            GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, desc 2 0)
            GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, desc 2 (2 * 4))
            GL.drawArrays GL.TriangleStrip 0 6
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Disabled

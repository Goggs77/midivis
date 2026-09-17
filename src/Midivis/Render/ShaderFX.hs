{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Statically embedded GLSL shaders (from @assets/shader@, spliced in at
--   compile time) plus lazily-compiled, cached 'Shader' handles for each
--   effect.
--
--   The program stays static: no runtime file access.  Shader compilation
--   happens once on first use and the GL program object is cached, so the
--   per-frame cost is just a uniform set + draw call.
module Midivis.Render.ShaderFX
        ( passVertex
        , shaderIdentity, shaderInvert, shaderGrayscale, shaderBlur
        , shaderTodo2
        , shaderPassUniform2f
        , shaderPassUniform1fv
        ) where

import Graphics.Gloss.Relative (Shader, compileShader, uniform2f, uniform1fv)

import qualified Data.Text as T
import Data.FileEmbed (embedFileRelative)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.IO.Unsafe (unsafePerformIO)

-- | Shared full-screen quad vertex shader.
passVertex :: Text
passVertex = decodeUtf8 $(embedFileRelative "assets/shader/pass.vert")

-- | Fragment shader sources, embedded at compile time.
fragIdentity   :: Text
fragIdentity   = decodeUtf8 $(embedFileRelative "assets/shader/identity.frag")

fragInvert     :: Text
fragInvert     = decodeUtf8 $(embedFileRelative "assets/shader/invert.frag")

fragGrayscale  :: Text
fragGrayscale  = decodeUtf8 $(embedFileRelative "assets/shader/grayscale.frag")

fragBlur       :: Text
fragBlur       = decodeUtf8 $(embedFileRelative "assets/shader/blur.frag")

-- | TODO 2 skeleton: interval-gap visualization (consumes uAngles[129]).
fragTodo2      :: Text
fragTodo2      = decodeUtf8 $(embedFileRelative "assets/shader/todo2.frag")

-- | Each shader is a separate top-level CAF, compiled once on first use
--   (inside the draw callback, so a GL context exists) and then cached by
--   the CAF's own lazy evaluation.
--
--   IMPORTANT: the per-shader cache must be a top-level CAF, NOT an IORef
--   hidden in the @where@ clause of a shared helper.  GHC's full-laziness
--   pass floats an argument-independent @unsafePerformIO (newIORef ...)@
--   out to the top level, so all four shaders would share ONE cache and
--   every effect would return the first-compiled program.  (That is exactly
--   why the picture locked on grayscale — the first shader forced happened
--   to be 'shaderGrayscale'.)

-- | Identity pass (no visual change; baseline / pipeline self-test).
shaderIdentity :: Shader
shaderIdentity = unsafePerformIO (compileShader (T.unpack passVertex) (T.unpack fragIdentity))
{-# NOINLINE shaderIdentity #-}

-- | Invert colours.
shaderInvert :: Shader
shaderInvert = unsafePerformIO (compileShader (T.unpack passVertex) (T.unpack fragInvert))
{-# NOINLINE shaderInvert #-}

-- | Grayscale (luminance-weighted desaturation).
shaderGrayscale :: Shader
shaderGrayscale = unsafePerformIO (compileShader (T.unpack passVertex) (T.unpack fragGrayscale))
{-# NOINLINE shaderGrayscale #-}

-- | 3x3 box blur.
shaderBlur :: Shader
shaderBlur = unsafePerformIO (compileShader (T.unpack passVertex) (T.unpack fragBlur))
{-# NOINLINE shaderBlur #-}

-- | TODO 2 skeleton shader (interval-gap visualization; passthrough until
--   the algorithm is filled in).  Reads uAngles[129] fed by Example.hs.
shaderTodo2 :: Shader
shaderTodo2 = unsafePerformIO (compileShader (T.unpack passVertex) (T.unpack fragTodo2))
{-# NOINLINE shaderTodo2 #-}

-- | Set a float uniform on a pass shader (e.g. effect parameters).
shaderPassUniform2f :: Shader -> String -> (Float, Float) -> IO ()
shaderPassUniform2f = uniform2f

-- | Set a float-array uniform on a pass shader (e.g. per-note data).
shaderPassUniform1fv :: Shader -> String -> [Float] -> IO ()
shaderPassUniform1fv = uniform1fv

module Graphics.Gloss.Relative.Internal.Data where

import Graphics.Gloss
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Rendering (Shader)

import Foreign.C.Types ( CInt, CFloat(..) )
import Foreign.Storable ( Storable(..), pokeByteOff, peekByteOff )

-- | A picture frame. Much like the original 'Picture' data type, but the purpose is to place and adjust pictures inside a frame with general dimensions.
data Frame
    -- | Create the largest possible frame with a desired aspect ratio within the current frame.
    = Aspect
        { aspectRatio :: Dimension -- ^ Aspect ratio dimensions.
        , aspectAlign :: Alignment -- ^ Alignment inside the parent frame.
        , aspectChild :: Frame -- ^ Child frame.
        }
    -- | Zoom the current frame to the given scale, producing a smaller frame.
    | Zoom
        { zoomX :: Unit -- ^ Horizontal scale.
        , zoomY :: Unit -- ^ Vertical scale.
        , zoomAlignment :: Alignment -- ^ Alignment inside the parent frame.
        , zoomChild :: Frame -- ^ Child frame.
        }
    -- | Split the current frame into a grid with the given numbers of rows and columns. Receives a matrix of frames, represented as a list of rows.
    | Grid [[Frame]]
    -- | Labels a frame region, to use in mouse events. Consider using @region@ or @preciseRegion@ instead of this constructor directly.
    | Region
        { regionName :: String -- ^ The label for the frame region. Does not need to be unique.
        , regionPrecise :: Bool -- ^ If 'True', transparent pixels do not count for frame region selection (pixel-perfect). If 'False', the whole rectangular region is used for selection. __Note:__ Use pixel-perfect regions with care, ideally only for smaller regions, as performance may suffer significantly.
        , regionChild :: Frame -- ^ Current frame.
        }
    -- | Overlay a sequence of frames, later ones drawn on top.
    | Overlay [Frame]
    -- | Stretch a picture to fill the frame, not preserving the picture's aspect ratio. __Note:__ Prefer the native frame combinators; use this (via @stretch@ \/ @fit@ \/ @native@) as a last resort, mainly for image files.
    | Stretch
        { stretchDimension :: Maybe Dimension -- ^ The explicit dimension of the picture, or inferred if 'Nothing'.
        , stretchPicture :: Picture -- ^ The picture to stretch to the frame's dimension.
        }
    -- | Advanced contructor. In case you need to know the exact screen size for a frame.
    | Sized (Dimension -> Frame)
    -- | Post-process a child frame with a GLSL shader.  The child is
    --   rendered offscreen to a render target, then the shader samples it
    --   as a full-screen quad.  Requires an OpenGL context (dynamic path).
    | FX
        { fxShader :: Shader -- ^ Program to run over the child's render.
        , fxSetup  :: Shader -> IO () -- ^ Uniform setup, run after the program is bound (per pass).
        , fxChild  :: Frame  -- ^ Frame whose render is the shader input.
        }

-- | 'Frame's cannot be derived-'Show'n because of the 'Sized' escape hatch (it holds a function).
-- This hand-written instance renders that single case as a placeholder.
instance Show Frame where
    showsPrec p frame = case frame of
        Aspect r a f    -> con "Aspect"  [arg r, arg a, arg f]
        Zoom x y a f    -> con "Zoom"    [arg x, arg y, arg a, arg f]
        Grid xs         -> con "Grid"    [arg xs]
        Region n t f    -> con "Region"  [arg n, arg t, arg f]
        Overlay xs      -> con "Overlay" [arg xs]
        Stretch md pic  -> con "Stretch" [arg md, arg pic]
        Sized _         -> con "Sized"   [showString "<dimension-dependent>"]
        FX _ _ f       -> con "FX"      [showString "<shader>", arg f]
      where
        arg :: Show a => a -> ShowS
        arg = showsPrec 11
        con name args = showParen (p > 10) $
            foldl (\acc a -> acc . showString " " . a) (showString name) args

instance Semigroup Frame where
    a <> b = Overlay [a,b]

instance Monoid Frame where
    mempty = Overlay []

-- * Alignments

-- | Alignment options for a picture or frame inside a larger frame.
data Alignment
    -- | Alignment of a child frame relative to its parent frame. Consider using the named helpers (@alignCenter@, @alignTopLeft@, ...) instead of this constructor directly.
    = Align
        { alignH :: HorizontalAlignment -- ^ Horizontal alignment.
        , alignV :: VerticalAlignment -- ^ Vertical alignment.
        }
    -- | Advanced contructor. Offset of the center of the child frame relative to the center of its parent frame. Consider using @alignAt@.
    -- __Note:__: The offset is truncated, so that the child frame will always remain within the parent frame.
    | AlignAt
        { alignPos :: UnitPoint -- ^ Alignment position of the center of a child frame inside a parent frame.
        }
    deriving (Eq,Ord,Show)

-- | A unit of measurement, either a fraction of the enclosing frame or an absolute number of pixels.
data Unit
    = Relative Float -- ^ A relative unit (a fraction of the enclosing frame, e.g. @0.5@ for half).
    | Absolute Float -- ^ An absolute unit (pixels).
    deriving (Eq,Ord,Show)

-- | A point whose coordinates are given as 'Unit's.
type UnitPoint = (Unit,Unit)

-- | 'Storable' layout of a 'Unit':
--   @[tag :: CInt][value :: CFloat]@, 8 bytes, 4-byte aligned.
--   @tag@ is 0 for 'Relative' and 1 for 'Absolute'.
instance Storable Unit where
    sizeOf _    = sizeOf (0 :: CInt) + sizeOf (0 :: CFloat)
    alignment _ = alignment (0 :: CFloat)
    poke p u = do
        let (tag, val) = case u of
                Relative x -> (0 :: CInt, x)
                Absolute x -> (1 :: CInt, x)
        pokeByteOff p 0 tag
        pokeByteOff p 4 (CFloat val)
    peek p = do
        tag <- peekByteOff p 0 :: IO CInt
        CFloat val <- peekByteOff p 4
        pure $ case tag of
            0 -> Relative val
            _ -> Absolute val

-- Note: 'UnitPoint' (= (Unit,Unit)) needs no hand-written instance:
-- @Foreign.Storable.Tuple@ already provides
-- @instance (Storable a, Storable b) => Storable (a, b)@ with the same
-- consecutive-element layout (sizeOf = sizeOf a + sizeOf b, alignment =
-- max of the two).  Import that module wherever a Storable UnitPoint is
-- required.

-- | Horizontal alignment options for a picture or frame inside a larger frame.
data HorizontalAlignment
    = AlignLeft
    | AlignCenter
    | AlignRight
    deriving (Eq,Ord,Show,Enum,Bounded)

-- | Vertical alignment options for a picture or frame inside a larger frame.
data VerticalAlignment
    = AlignTop
    | AlignMiddle
    | AlignBottom
    deriving (Eq,Ord,Show,Enum,Bounded)

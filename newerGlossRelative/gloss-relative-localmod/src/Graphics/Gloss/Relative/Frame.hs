{-# LANGUAGE ViewPatterns #-}

-- | The core functionality of the @gloss-relative@ package is exported in this module.
-- This includes the 'Frame' abstraction and helper functions, together with the whole
-- Gloss picture-building vocabulary ('Picture', @Color@, points and vectors), so that this
-- single module is enough to build any frame without importing @Graphics.Gloss@ directly.
module Graphics.Gloss.Relative.Frame
    ( -- * Re-exported Gloss picture vocabulary
      module Graphics.Gloss.Data.Picture
    , module Graphics.Gloss.Data.Color
    , module Graphics.Gloss.Data.Bitmap
    , module Graphics.Gloss.Data.Point
    , module Graphics.Gloss.Data.Vector
      -- * The frame type
    , Frame(..), Dimension, Unit(..), UnitPoint
    , Alignment(..), HorizontalAlignment(..), VerticalAlignment(..)
      -- * Rendering
    , renderStaticFrame
      -- * Leaf frames
    , empty, solid, outline, ellipse, solidEllipse, thickEllipse, shape, stroke, banner
      -- * Embedding raw pictures (last resort)
    , stretch, fit, native, nativeSized
      -- * Layout and combination
    , overlay, row, column, grid, splitLeft, splitRight, splitTop, splitBottom, zoom, aspect
      -- * Decorators
    , background, inset, border, bordered, marginLeft, marginRight, marginTop, marginBottom, margins
      -- * Regions (mouse events)
    , region, preciseRegion
      -- * Shader post-processing
    , fx, fxWith
    , Shader, compileShader, uniform1f, uniform1i, uniform2f, uniform1fv
      -- * Alignments
    , alignTopLeft, alignTop, alignTopRight, alignLeft, alignCenter, alignRight
    , alignBottomLeft, alignBottom, alignBottomRight, alignAt
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Data
import Graphics.Gloss.Relative.Internal.Frame
import Graphics.Gloss.Rendering (Shader, compileShader, uniform1f, uniform1i, uniform2f, uniform1fv)

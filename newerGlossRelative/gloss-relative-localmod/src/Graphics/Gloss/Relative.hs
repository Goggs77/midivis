-- | Module that wraps all the functionality of @gloss-relative@ and the core picture-building
-- functionality of @gloss@.
--
-- This is a drop-in replacement for @import Graphics.Gloss@: it re-exports the whole Gloss
-- picture vocabulary (via "Graphics.Gloss.Relative.Frame") but /replaces/ Gloss's entry points
-- (@display@, @play@, @animate@, @simulate@) with the frame-based 'displayRelative',
-- 'playRelative' and 'animateRelative' (see "Graphics.Gloss.Relative.Interface"). Gloss's own
-- entry points are intentionally not re-exported, to avoid name overloading.
module Graphics.Gloss.Relative
    ( module Graphics.Gloss.Relative.Frame
    , module Graphics.Gloss.Relative.Interface
    ) where

import Graphics.Gloss.Relative.Frame
import Graphics.Gloss.Relative.Interface

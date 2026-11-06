module Graphics.Gloss.Relative.Internal.Dimension where

import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Environment

import Control.Monad

-- | A dimension is pair @(width,height)@.
-- When used as a screen size, the position @(0,0)@ is considered to be at the center.
type Dimension = (Float,Float)

-- | A screen size screen size is a 'Dimension' in pixels, where the position @(0,0)@ is at the center.
type ScreenSize = (Int,Int)

-- | Gets the dimension of a Gloss 'Display'.
getDisplayScreenSize :: Display -> IO ScreenSize 
getDisplayScreenSize (InWindow _ dim _) = return dim
getDisplayScreenSize (FullScreen) = getScreenSize

getDisplayDimension :: Display -> IO Dimension 
getDisplayDimension = liftM screenSizeToDimension . getDisplayScreenSize

screenSizeToDimension :: ScreenSize -> Dimension
screenSizeToDimension (w,h) = (realToFrac w,realToFrac h)

dimensionToScreenSize :: Dimension -> ScreenSize
dimensionToScreenSize (w,h) = (ceiling w,ceiling h)
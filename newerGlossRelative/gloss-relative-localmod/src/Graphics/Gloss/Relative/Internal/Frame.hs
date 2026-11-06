{-# LANGUAGE ViewPatterns #-}

module Graphics.Gloss.Relative.Internal.Frame where

import Graphics.Gloss
import qualified Graphics.Gloss.Rendering as Gloss
import Graphics.Gloss.Rendering (Shader)
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Data
import qualified Graphics.Gloss.Relative.Internal.Picture as Picture
import qualified Graphics.Gloss.Relative.Internal.Window as Window
import qualified Graphics.Gloss.Relative.Internal.Raster as Raster
import qualified Graphics.Gloss.Relative.Internal.Cache as Cache

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Identity (Identity(..))
import qualified Control.Monad.Identity as Identity
import Data.IORef

-- | Renders a frame into a picture.
renderStaticFrame
    :: Frame -- ^ The frame to render. __Note:__ This function ignores frame regions. Use only if you don't need mouse hover events in this frame.
    -> Dimension -- ^ The dimension of the screen in which to render the frame.
    -> Picture -- ^ The resulting picture.
renderStaticFrame f screen = Picture.flattenPicture $ Window.wPic $ renderStaticFrameOutput f screen

-- | Like 'renderStaticFrame', but also returns the region handler.
--
-- Regions are hit-tested as plain rectangles here (pixel-perfect ones are not rasterized), since
-- rasterization needs an OpenGL context. This is used to pre-populate the region handler before the
-- first frame is drawn, so that mouse events arriving early already see the frame's regions.
renderStaticFrameOutput :: Frame -> Dimension -> Window.WindowOutput
renderStaticFrameOutput f screen = Identity.runIdentity $ Window.execWindow screen (renderStaticFrameAsWindow f)
    where
    renderStaticFrameAsWindow :: Monad m => Frame -> Window.Window m ()
    renderStaticFrameAsWindow (Aspect (aw,ah) a f) = do
        dim <- Window.askDimension
        let adim = Window.largestAspectFitDim aw ah dim
        wo <- lift $ Window.execWindow adim (renderStaticFrameAsWindow f)
        wo' <- lift $ Window.execWindow adim $ Window.fitWith adim wo
        Window.alignWith adim a wo'
    renderStaticFrameAsWindow (Zoom x y a f) = do
        (w,h) <- Window.askDimension
        let dim' = (w * Picture.resolveZoom w x, h * Picture.resolveZoom h y)
        wo <- lift $ Window.execWindow dim' (renderStaticFrameAsWindow f)
        Window.alignWith dim' a wo
    renderStaticFrameAsWindow (Grid xs) = do
        Window.grid (map (map renderStaticFrameAsWindow) xs)
        return ()
    renderStaticFrameAsWindow (Overlay xs) = mapM_ renderStaticFrameAsWindow xs
    renderStaticFrameAsWindow (Stretch Nothing pic) = Window.stretch pic
    renderStaticFrameAsWindow (Stretch (Just dim) pic) = Window.stretchWith dim pic
    renderStaticFrameAsWindow (Sized f) = do
        dim <- Window.askDimension
        renderStaticFrameAsWindow (f dim)
    -- pixel-perfect regions degrade to rectangles: rasterizing needs an OpenGL context
    renderStaticFrameAsWindow (Region name _ f) = Window.addRegionRectangle name >> renderStaticFrameAsWindow f

renderDynamicFrame :: Int -> Cache.CacheTable -> Gloss.State -> IORef (IO Raster.Offscreen) -> Frame -> Dimension -> IO Window.WindowOutput
renderDynamicFrame i cache state off f screen = do
    wo <- Window.execWindow screen (renderDynamicFrameAsWindow f)
    return $ wo { Window.wPic = Picture.flattenPicture (Window.wPic wo) }
    where
    renderDynamicFrameAsWindow :: Frame -> Window.Window IO ()
    renderDynamicFrameAsWindow (Aspect (aw,ah) a f) = do
        dim <- Window.askDimension
        let adim = Window.largestAspectFitDim aw ah dim
        wo <- lift $ Window.execWindow adim (renderDynamicFrameAsWindow f)
        wo' <- lift $ Window.execWindow adim $ Window.fitWith adim wo
        Window.alignWith adim a wo'
    renderDynamicFrameAsWindow (Zoom x y a f) = do
        (w,h) <- Window.askDimension
        let dim' = (w * Picture.resolveZoom w x, h * Picture.resolveZoom h y)
        wo <- lift $ Window.execWindow dim' (renderDynamicFrameAsWindow f)
        Window.alignWith dim' a wo
    renderDynamicFrameAsWindow (Grid xs) = do
        Window.grid (map (map renderDynamicFrameAsWindow) xs)
        return ()
    renderDynamicFrameAsWindow (Overlay xs) = mapM_ renderDynamicFrameAsWindow xs
    renderDynamicFrameAsWindow (Stretch Nothing pic) = Window.stretch pic
    renderDynamicFrameAsWindow (Stretch (Just dim) pic) = Window.stretchWith dim pic
    renderDynamicFrameAsWindow (Sized f) = do
        dim <- Window.askDimension
        renderDynamicFrameAsWindow (f dim)
    renderDynamicFrameAsWindow (Region name False f) = Window.addRegionRectangle name >> renderDynamicFrameAsWindow f
    renderDynamicFrameAsWindow (Region name True f) = Cache.cachedRenderFrame i cache renderRegion name f
        where
        renderRegion :: String -> Frame -> Window.Window IO ()
        renderRegion name f = do
            dim <- Window.askDimension
            let screen = dimensionToScreenSize dim
            o <- liftIO $ Raster.getResizeOffscreen off screen
            (Window.WindowOutput pic region) <- lift $ Window.execWindow dim (renderDynamicFrameAsWindow f)
            pic' <- liftIO $ Raster.rasterPicture state o screen (Picture.flattenPicture pic)
            Window.addRegionTransparent name pic'
            Window.tellWindowOutput (Window.WindowOutput (Raster.fromRasteredPicture pic') region)
    -- FX: rasterize the child to an offscreen RT, then run the shader over
    -- it as a full-screen textured quad.  The result is returned as a
    -- bitmap Picture (reusing the offscreen pool) so it composites into the
    -- rest of the frame as usual.
    renderDynamicFrameAsWindow (FX sh setup f) = do
        dim <- Window.askDimension
        let screen = dimensionToScreenSize dim
        o <- liftIO $ Raster.getResizeOffscreen off screen
        (Window.WindowOutput pic region) <- lift $ Window.execWindow dim (renderDynamicFrameAsWindow f)
        pic' <- liftIO $ Raster.rasterPictureFX state o sh setup screen (Picture.flattenPicture pic)
        Window.tellWindowOutput (Window.WindowOutput pic' region)

-- * Alignments

-- | Alignment to the top-left of the frame.
alignTopLeft :: Alignment
alignTopLeft = Align AlignLeft AlignTop

-- | Alignment to the top (and center) of the frame.
alignTop :: Alignment
alignTop = Align AlignCenter AlignTop

-- | Alignment to the top-right of the frame.
alignTopRight :: Alignment
alignTopRight = Align AlignRight AlignTop

-- | Alignment to the left (and middle) the frame.
alignLeft :: Alignment
alignLeft = Align AlignLeft AlignMiddle

-- | Alignment to the center (and middle) the frame.
alignCenter :: Alignment
alignCenter = Align AlignCenter AlignMiddle

-- | Alignment to the right (and middle) the frame.
alignRight :: Alignment
alignRight = Align AlignRight AlignMiddle

-- | Alignment to the bottom-left the frame.
alignBottomLeft :: Alignment
alignBottomLeft = Align AlignLeft AlignBottom

-- | Alignment to the bottom (and middle) the frame.
alignBottom :: Alignment
alignBottom = Align AlignCenter AlignBottom

-- | Alignment to the bottom-right the frame.
alignBottomRight :: Alignment
alignBottomRight = Align AlignRight AlignBottom

-- | Aligns the center of the child frame at a given offset from the center of the parent frame.
-- The offset is truncated so that the child frame remains within the parent.
alignAt :: UnitPoint -> Alignment
alignAt = AlignAt

-- * Leaf frames

-- | The empty frame (draws nothing). Identity element for 'overlay' and '<>'.
empty :: Frame
empty = Overlay []

-- | Fills the frame with a solid color.
solid :: Color -> Frame
solid c = Stretch Nothing $ Color c $ rectangleSolid 10 10 -- any dimension would work here

-- | Paints the rectangular outline of the frame with a color.
outline :: Color -> Frame
outline c = Stretch Nothing $ Color c $ rectangleWire 10 10 -- any dimension would work here

-- | Draws an ellipse fitting the current frame.
ellipse :: Color -> Frame
ellipse c = nativeSized $ \(w,h) ->
    let (diameter,sx,sy) = if w < h then (w,1,h/w) else (h,w/h,1)
    in Scale sx sy $ Color c $ Circle (diameter/2)

-- | Draws an ellipse fitting the current frame, with a given outline thickness.
thickEllipse :: Color -> Unit -> Frame
thickEllipse c thicku = nativeSized $ \(w,h) ->
    let (diameter,sx,sy) = if w < h then (w,1,h/w) else (h,w/h,1)
        thick = Picture.resolveUnit diameter thicku
    in Scale sx sy $ Color c $ ThickCircle (diameter/2 - thick/2) thick

-- | Draws an ellipse fitting the current frame, filled with a given color.
solidEllipse :: Color -> Frame
solidEllipse c = nativeSized $ \(w,h) ->
    let (diameter,sx,sy) = if w < h then (w,1,h/w) else (h,w/h,1)
        radius = diameter / 2
    in Scale sx sy $ Color c $ ThickCircle (radius/2) radius

-- | A convex polygon filled with a solid color.
shape
    :: [UnitPoint] -- ^ A sequence of points that form the polygon, with origin at the center of the frame. Differently from Gloss 'polygon', each coordinate is a 'Unit': @'Relative' 0.5@ is the right\/top edge, @'Relative' (-0.5)@ the left\/bottom edge, and @'Absolute' px@ an offset in pixels from the center.
    -> Color -- ^ The fill color.
    -> Frame -- ^ The resulting frame
shape ps c = nativeSized $ \dim -> Color c $ Polygon $ map (Picture.resolveUnitPoint dim) ps

-- | A line connecting a sequence of points, drawn with a color. Point coordinates are 'Unit's, as in 'shape'.
stroke
    :: [UnitPoint] -- ^ A sequence of points that form the line.
    -> Color -- ^ The line color.
    -> Frame -- ^ The resulting frame
stroke ps c = nativeSized $ \dim -> Color c $ Line $ map (Picture.resolveUnitPoint dim) ps

-- | Draws a banner, that is, a piece of text fitted inside the frame, with a color.
banner :: String -> Color -> Frame
banner txt c = Sized $ \dim ->
    let pic = Window.wPic $ Identity.runIdentity $ Window.execWindow dim (Window.text txt)
    in Stretch (Just dim) $ Color c pic

-- * Embedding raw pictures (last resort)

-- | Stretches a picture to fill the frame, not preserving its aspect ratio.
-- __Note:__ Prefer the native frame combinators; use this mainly for image files.
stretch :: Picture -> Frame
stretch pic = Stretch Nothing pic

-- | Fits a picture inside the frame, preserving its aspect ratio and centering it.
-- __Note:__ Prefer the native frame combinators; use this mainly for image files.
fit :: Picture -> Frame
fit pic = Aspect picdim alignCenter (Stretch (Just picdim) pic)
    where picdim = Picture.pictureDimension pic

-- | Draws a picture inside a frame using its original pixel dimensions, with no scaling, stretching or fitting.
-- __Warning:__ May naturally lead to misaligned pictures if not used with care.
native :: Picture -> Frame
native pic = Sized $ \dim -> Stretch (Just dim) pic

-- | Like 'native', but the picture may depend on the resolved dimension of the current frame.
nativeSized :: (Dimension -> Picture) -> Frame
nativeSized fpic = Sized $ \dim -> Stretch (Just dim) (fpic dim)

-- * Layout and combination

-- | Overlays a sequence of frames, later ones drawn on top. See also the 'Semigroup' instance ('<>').
overlay :: [Frame] -> Frame
overlay = Overlay

-- | Lays out frames left-to-right in a single row, evenly splitting the width.
row :: [Frame] -> Frame
row xs = Grid [xs]

-- | Lays out frames top-to-bottom in a single column, evenly splitting the height.
column :: [Frame] -> Frame
column xs = Grid (map (\x -> [x]) xs)

-- | Creates each cell in a grid depending on the row and column indexes.
grid :: Int -> Int -> (Int -> Int -> Frame) -> Frame
grid ncols nrows mk = Grid $ map (\row -> map (\col -> mk row col) [0..ncols-1]) [0..nrows-1]

-- | Smart constructor for 'Zoom'.
zoom :: Unit -> Unit -> Alignment -> Frame -> Frame
zoom = Zoom

-- | Smart constructor for 'Aspect'.
aspect :: Dimension -> Alignment -> Frame -> Frame
aspect = Aspect

-- | Splits the frame into a left pane of the given width and a right pane filling the rest.
splitLeft :: Unit -> Frame -> Frame -> Frame
splitLeft u a b = Sized $ \(w,h) ->
    let wl = Picture.resolveUnit w u
    in Overlay
        [ Zoom (Absolute wl)      (Absolute h) alignLeft  a
        , Zoom (Absolute (w-wl))  (Absolute h) alignRight b ]

-- | Splits the frame into a right pane of the given width and a left pane filling the rest.
splitRight :: Unit -> Frame -> Frame -> Frame
splitRight u a b = Sized $ \(w,h) ->
    let wr = Picture.resolveUnit w u
    in Overlay
        [ Zoom (Absolute (w-wr)) (Absolute h) alignLeft  a
        , Zoom (Absolute wr)     (Absolute h) alignRight b ]

-- | Splits the frame into a top pane of the given height and a bottom pane filling the rest.
splitTop :: Unit -> Frame -> Frame -> Frame
splitTop u a b = Sized $ \(w,h) ->
    let ht = Picture.resolveUnit h u
    in Overlay
        [ Zoom (Absolute w) (Absolute ht)     alignTop    a
        , Zoom (Absolute w) (Absolute (h-ht)) alignBottom b ]

-- | Splits the frame into a bottom pane of the given height and a top pane filling the rest.
splitBottom :: Unit -> Frame -> Frame -> Frame
splitBottom u a b = Sized $ \(w,h) ->
    let hb = Picture.resolveUnit h u
    in Overlay
        [ Zoom (Absolute w) (Absolute (h-hb)) alignTop    a
        , Zoom (Absolute w) (Absolute hb)     alignBottom b ]

-- * Decorators

-- | Places a solid color behind a frame.
background :: Color -> Frame -> Frame
background c f = Overlay [solid c, f]

-- | Shrinks a frame by adding a uniform margin on all four sides.
inset :: Unit -> Frame -> Frame
inset u = margins u u u u

-- | Overlays a rectangular border of the given thickness on the frame's edge,
-- on top of the child and without resizing it. See 'bordered' to also inset the child.
border :: Unit -> Color -> Frame -> Frame
border u c f = Overlay [f, edges]
    where
    edges = Sized $ \(w,h) ->
        let tw = Picture.resolveUnit w u
            th = Picture.resolveUnit h u
        in Overlay
            [ Zoom (Absolute tw) (Absolute h)  alignLeft   (solid c)
            , Zoom (Absolute tw) (Absolute h)  alignRight  (solid c)
            , Zoom (Absolute w)  (Absolute th) alignTop    (solid c)
            , Zoom (Absolute w)  (Absolute th) alignBottom (solid c) ]

-- | Draws a border of the given thickness around a frame, insetting the child by the same amount so
-- its content clears the border. Equivalent to @'border' u c . 'inset' u@.
bordered :: Unit -> Color -> Frame -> Frame
bordered u c f = border u c (inset u f)

-- | Reduces the current frame by adding a left margin.
marginLeft :: Unit -> Frame -> Frame
marginLeft u f = Sized $ \(w,h) -> Zoom (Absolute (w - Picture.resolveUnit w u)) (Absolute h) alignRight f

-- | Reduces the current frame by adding a right margin.
marginRight :: Unit -> Frame -> Frame
marginRight u f = Sized $ \(w,h) -> Zoom (Absolute (w - Picture.resolveUnit w u)) (Absolute h) alignLeft f

-- | Reduces the current frame by adding a top margin.
marginTop :: Unit -> Frame -> Frame
marginTop u f = Sized $ \(w,h) -> Zoom (Absolute w) (Absolute (h - Picture.resolveUnit h u)) alignBottom f

-- | Reduces the current frame by adding a bottom margin.
marginBottom :: Unit -> Frame -> Frame
marginBottom u f = Sized $ \(w,h) -> Zoom (Absolute w) (Absolute (h - Picture.resolveUnit h u)) alignTop f

-- | Reduces the current frame by adding margins on all four sides.
margins
    :: Unit -- ^ Left margin.
    -> Unit -- ^ Right margin.
    -> Unit -- ^ Top margin.
    -> Unit -- ^ Bottom margin.
    -> Frame -- ^ The inner frame.
    -> Frame -- ^ The resulting frame.
margins l r t b f = marginLeft l $ marginRight r $ marginTop t $ marginBottom b f

-- * Regions (mouse events)

-- | Tags a frame as a named region for mouse events, using its whole rectangle for hit-testing (cheap).
region :: String -> Frame -> Frame
region name f = Region name False f

-- | Tags a frame as a named region for mouse events, using pixel-perfect (non-transparent) hit-testing.
-- __Note:__ More expensive than 'region'; use for smaller regions.
preciseRegion :: String -> Frame -> Frame
preciseRegion name f = Region name True f

-- | Post-process a frame with a GLSL shader.  The child frame is rendered
--   offscreen and the shader runs over it as a full-screen textured quad
--   (sampling texture unit 0; @uTexSize@ is set to the pixel size).
--   Requires the dynamic rendering path (e.g. 'playRelativeIO').
--
--   Example:
--
--   @
--   sh <- compileShader vertSrc invertSrc
--   ... fx sh (solid blue)
--   @
fx :: Shader -> Frame -> Frame
fx sh = fxWith sh (const (return ()))

-- | Like 'fx', but runs @setup@ after the shader program is bound and before
--   the quad is drawn — the only place per-frame uniforms take effect.  Use
--   it to feed the shader per-frame data (e.g. 'uniform1fv' arrays).
fxWith :: Shader -> (Shader -> IO ()) -> Frame -> Frame
fxWith sh setup f = FX sh setup f

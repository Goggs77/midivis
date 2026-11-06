module Graphics.Gloss.Relative.Internal.Picture where

import Graphics.Gloss
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Data

-- * Units

-- | Resolves a 'Unit' into an absolute length in pixels, given a reference length
-- (typically the width or height of the enclosing frame).
resolveUnit :: Float -> Unit -> Float
resolveUnit ref (Relative p) = p * ref
resolveUnit _   (Absolute x) = x

-- | Resolves a 'Unit' into a scale /factor/ (a fraction of the reference length),
-- given the reference length. Used for zooming.
resolveZoom :: Float -> Unit -> Float
resolveZoom _   (Relative p)  = p
resolveZoom ref (Absolute px) = if ref == 0 then 0 else px / ref

-- | Resolves a 'UnitPoint' into an absolute 'Point', given the enclosing dimension.
resolveUnitPoint :: Dimension -> UnitPoint -> Point
resolveUnitPoint (w,h) (ux,uy) = (resolveUnit w ux, resolveUnit h uy)

-- * Pictures

translateX :: Float -> Picture -> Picture
translateX x p = Translate x 0 p

translateY :: Float -> Picture -> Picture
translateY y p = Translate 0 y p

-- The default Monoid instance for 'Picture' is buggy, since Blank is not in fact a neutral element.
emptyPicture :: Picture
emptyPicture = Pictures []

joinPicture :: Picture -> Picture -> Picture
joinPicture (Pictures xs) (Pictures ys) = catPictures $ xs ++ ys
joinPicture x (Pictures ys) = catPictures (x : ys)
joinPicture (Pictures xs) y = catPictures (xs ++ [y])
joinPicture x y = catPictures [x,y]

catPictures :: [Picture] -> Picture
catPictures [x] = x
catPictures xs = Pictures xs

-- | Flattens a picture, by eliminating nested transforms.
--
-- __Note:__ OpenGL backends have a nested transform stack of only 32, so we need to make sure to
-- flatten pictures before rendering to avoid stack overflows. Frames naturally build deeply nested
-- transforms (a grid of @n@ rows nests @n@ translations), so without this a moderately sized layout
-- (around a @16x16@ grid) would overflow the stack. Flattening also reduces the number of OpenGL
-- matrix operations per frame.
flattenPicture :: Picture -> Picture
flattenPicture p = catPictures $ flatten identityMatrix p

-- Representing Affine Transformation Matrices.
-- | a c e |
-- | b d f |
-- | 0 0 1 |
data Matrix = Matrix Float Float Float Float Float Float

identityMatrix :: Matrix
identityMatrix = Matrix 1 0 0 1 0 0

multiplyMatrix :: Matrix -> Matrix -> Matrix
multiplyMatrix (Matrix a1 b1 c1 d1 e1 f1) (Matrix a2 b2 c2 d2 e2 f2) =
    Matrix (a1*a2 + c1*b2) (b1*a2 + d1*b2)
           (a1*c2 + c1*d2) (b1*c2 + d1*d2)
           (a1*e2 + c1*f2 + e1) (b1*e2 + d1*f2 + f1)

flatten :: Matrix -> Picture -> [Picture]
flatten _ Blank = []
flatten m (Pictures ps) = concatMap (flatten m) ps
-- recurse under a colour, so that colouring a composite does not stop the flattening
flatten m (Color c p) = case flatten m p of
    []  -> []
    ps  -> [Color c (catPictures ps)]
flatten m (Translate x y p) = flatten (m `multiplyMatrix` Matrix 1 0 0 1 x y) p
flatten m (Scale sx sy p) = flatten (m `multiplyMatrix` Matrix sx 0 0 sy 0 0) p
flatten m (Rotate deg p) =
        let rad = -deg * pi / 180 -- Gloss is clockwise
            s = sin rad
            c = cos rad
        in flatten (m `multiplyMatrix` Matrix c (-s) s c 0 0) p
-- paths can absorb the transform exactly, leaving no transform nodes at all
flatten m (Polygon points) = [Polygon (map (transformPoint m) points)]
flatten m (Line points) = [Line (map (transformPoint m) points)]
flatten m pic = [transformLeaf m pic]

transformPoint :: Matrix -> Point -> Point
transformPoint (Matrix a b c d e f) (x, y) =
    (a*x + c*y + e, b*x + d*y + f)

-- | Re-expresses an arbitrary affine matrix using the transforms Gloss actually provides.
--
-- Gloss has no shear primitive, so a QR-style @Translate . Rotate . Scale@ decomposition is only
-- exact for non-sheared matrices (shear arises from a non-uniform 'Scale' composed with a
-- 'Rotate'). We instead use the SVD @M = R(theta) . diag(s1,s2) . R(phi)@, which is exact for
-- /every/ affine matrix, so no shear special-case is needed.
transformLeaf :: Matrix -> Picture -> Picture
transformLeaf (Matrix a b c d e f) leaf =
    Translate e f $ Rotate (degrees theta) $ Scale s1 s2 $ Rotate (degrees phi) leaf
    where
    -- closed-form 2x2 SVD
    e0 = (a + d) / 2
    f0 = (a - d) / 2
    g0 = (b + c) / 2
    h0 = (b - c) / 2
    q  = sqrt (e0*e0 + h0*h0)
    r  = sqrt (f0*f0 + g0*g0)
    s1 = q + r
    s2 = q - r
    a1 = atan2 g0 f0
    a2 = atan2 h0 e0
    theta = (a2 + a1) / 2
    phi   = (a2 - a1) / 2
    degrees x = x * 180 / pi

-- * Boxes

-- | A rectangular box within the screen.
data Box = Box
    { boxLeftTop :: Point
    , boxDimension :: Dimension
    } deriving (Eq,Ord,Show)

instance Semigroup Box where
    (Box (x1,y1) (w1,h1)) <> (Box (x2,y2) (w2,h2)) = Box (xmin,ymin) (abs (xmax-xmin),abs (ymax-ymin))
         where xmin = min x1 x2
               ymin = min y1 y2
               xmax = max (x1+w1) (x2+w2)
               ymax = max (y1+h1) (y2+h2)

instance Monoid Box where
    mempty = Box (0,0) (0,0)

boxScreenSize :: Box -> ScreenSize
boxScreenSize (Box (x,y) (xdim,ydim)) = (ceiling $ maxX * 2,ceiling $ maxY * 2)
    where
    maxX = max (abs x) (abs $ x + xdim)
    maxY = max (abs y) (abs $ y + ydim)

dimensionToBox :: Dimension -> Box
dimensionToBox (w,h) = Box { boxLeftTop = (-w/2,h/2), boxDimension = (w,h) }

pointInsideBox :: Point -> Box -> Bool
pointInsideBox (mouseX,mouseY) r =
    let (boxLeft,boxTop) = boxLeftTop r
        (w,h) = boxDimension r
        boxRight = boxLeft + w
        boxBottom = boxTop - h
    in (boxLeft <= mouseX && mouseX <= boxRight) && (boxBottom <= mouseY && mouseY <= boxTop)

-- converts a point inside a box to a relative point within the box
pointWithinBox :: Point -> Box -> Maybe Point
pointWithinBox p@(x,y) r = if pointInsideBox p r
    then
        let (boxLeft,boxTop) = boxLeftTop r
            (w,h) = boxDimension r
            boxCenterX = boxLeft + w / 2
            boxCenterY = boxTop - h / 2
        in Just (x - boxCenterX,y - boxCenterY)
    else Nothing

translateBoxX :: Float -> Box -> Box
translateBoxX f r = r { boxLeftTop = translatePointX f (boxLeftTop r) }

translateBoxY :: Float -> Box -> Box
translateBoxY f r = r { boxLeftTop = translatePointY f (boxLeftTop r) }

-- * Picture bounding box

-- | Estimates the bounding @Box@ of a 'Picture'.
pictureBox :: Picture -> Box
pictureBox = bbToBox . bbox

pictureScreenSize :: Picture -> ScreenSize
pictureScreenSize = boxScreenSize . pictureBox

pictureDimension :: Picture -> Dimension
pictureDimension = screenSizeToDimension . pictureScreenSize

type Ellipse = (Float,Float,Float,Float,Float,Float)
data BB = BB { bbEllipse :: [Ellipse], bbPoints :: [Point] } deriving Show

instance Semigroup BB where
    (BB es1 ps1) <> (BB es2 ps2) = BB (es1++es2) (ps1++ps2)
instance Monoid BB where
    mempty = BB [] []

bbox :: Picture -> BB
bbox Blank = mempty
bbox (Polygon l) = BB [] l
bbox (Line l) = BB [] l
bbox (Circle r) = case (circEllipse r) of
    Nothing -> mempty
    Just e -> BB [e] []
bbox (ThickCircle r thick) = case (circEllipse $ r + thick / 2) of
    Nothing -> mempty
    Just e -> BB [e] []
bbox (Arc _ _ r) = bbox (Circle r) -- big overapproximation
bbox (ThickArc _ _ r thick) = bbox (ThickCircle r thick) -- big overapproximation
bbox (Text str) = bbox $ Translate (textWidth/2) (fromIntegral charHeight/2) rect
    where
    rect = rectangleWire textWidth (realToFrac charHeight)
    textWidth = realToFrac (length str) * (realToFrac charWidth)
bbox (Bitmap bmp) = bbox $ rectangleWire (fromIntegral w) (fromIntegral h)
    where (w,h) = bitmapSize bmp
bbox (BitmapSection rect bmp) = bbox $ rectangleWire (fromIntegral w) (fromIntegral h)
    where (w,h) = rectSize rect
bbox (LineStripBuffer _ _) = bbox $ rectangleWire 2 2
bbox (Color c p) = bbox p
bbox (Scale x y p) = scaleBB x y (bbox p)
bbox (Translate x y p) = translateBB x y (bbox p)
bbox (Rotate alpha p) = rotateBB alpha (bbox p)
bbox (Pictures xs) = mconcat $ map bbox xs

charHeight :: Int
charHeight =  104
charWidth :: Int
charWidth = 70

scaleBB :: Float -> Float -> BB -> BB
scaleBB x y (BB es ps) = BB (map (scaleEllipse x y) es) (scalePath x y ps)

translateBB :: Float -> Float -> BB -> BB
translateBB x y (BB es ps) = BB (map (translateEllipse x y) es) (translatePath x y ps)

rotateBB :: Float -> BB -> BB
rotateBB alpha (BB es ps) = BB (map (rotateEllipse alpha) es) (rotatePath alpha ps)

bbToBox :: BB -> Box
bbToBox (BB es ps) = mconcat $ pathBox ps : map ellipseBox es

pathBox :: Path -> Box
pathBox [] = mempty
pathBox l = Box (xmin,ymin) (abs (xmax-xmin),abs (ymax-ymin))
 where (xs,ys) = unzip l
       xmin = minimum xs
       ymin = minimum ys
       xmax = maximum xs
       ymax = maximum ys

ellipseBox :: Ellipse -> Box
ellipseBox (a,b,c,d,e,f) | dlt==0 = mempty
                        | otherwise = Box (xl,yb) (abs (xr-xl),abs (yt-yb))
 where dlt = 4*a*c - b^2
       xc = (b*e - 2*c*d) / dlt
       yc = (b*d - 2*a*e) / dlt
       xr = xc + sqrt ((2*b*e-4*c*d)^2+4*dlt*(e^2-4*c*f)) / (2*dlt)
       yt = yc + sqrt ((2*b*d-4*a*e)^2+4*dlt*(d^2-4*a*f)) / (2*dlt)
       xl = xc - sqrt ((2*b*e-4*c*d)^2+4*dlt*(e^2-4*c*f)) / (2*dlt)
       yb = yc - sqrt ((2*b*d-4*a*e)^2+4*dlt*(d^2-4*a*f)) / (2*dlt)

-- converts point from cartesian to polar coordinates
toPolar :: Point -> (Float,Float)
toPolar (x,y) = (sqrt (x^2+y^2), atan2 y x)

-- converts point from polar to cartesian
toCartesian :: (Float,Float) -> Point
toCartesian (r,gama) = (r*cos gama,r*sin gama)

-- a circle as an ellipse
circEllipse :: Float -> Maybe Ellipse
circEllipse 0 = Nothing
circEllipse r = Just (1/r^2,0,1/r^2,0,0,-1)

rotatePoint :: Float -> Point -> Point
rotatePoint alpha (x,y) = toCartesian (r,gama + (rad (-alpha)))
 where (r,gama) = toPolar (x,y)

rotatePath :: Float -> Path -> Path
rotatePath alpha = map (rotatePoint alpha)

rotateEllipse :: Float -> Ellipse -> Ellipse
rotateEllipse alpha (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where theta = rad (alpha)
       a' = a*(cos theta)^2 + b*sin theta*cos theta + c*(sin theta)^2
       b' = 2*(c-a)*sin theta*cos theta + b*((cos theta)^2-(sin theta)^2)
       c' = a*(sin theta)^2 - b*sin theta*cos theta + c*(cos theta)^2
       d' = d*cos theta + e*sin theta
       e' = -d*sin theta + e*cos theta
       f' = f

translatePoint :: Float -> Float -> Point -> Point
translatePoint x y (a,b) = (a+x,b+y)

translatePointX :: Float -> Point -> Point
translatePointX f (x,y) = (x+f,y)

translatePointY :: Float -> Point -> Point
translatePointY f (x,y) = (x,y+f)

translatePath :: Float -> Float -> Path -> Path
translatePath x y = map (translatePoint x y)

translateEllipse :: Float -> Float -> Ellipse -> Ellipse
translateEllipse x y (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where a' = a
       b' = b
       c' = c
       d' = d - 2*a*x - b*y
       e' = e - b*x - 2*c*y
       f' = f + a*x^2 + b*x*y + c*y^2 - d*x -e*y

scalePoint :: Float -> Float -> Point -> Point
scalePoint xs ys (x,y) = (x*xs,y*ys)

scalePath :: Float -> Float -> Path -> Path
scalePath xs ys = map (scalePoint xs ys)

scaleEllipse :: Float -> Float -> Ellipse -> Ellipse
scaleEllipse xs ys (a,b,c,d,e,f) = (a',b',c',d',e',f')
 where a' = a / (xs^2)
       b' = b / (xs * ys)
       c' = c / (ys^2)
       d' = d / xs
       e' = e / ys
       f' = f

-- * Auxiliary functions

-- | Converts degrees to radians
rad :: Float -> Float
rad alpha = alpha * pi / 180

-- | Converts radians to degrees
deg :: Float -> Float
deg alpha = alpha * 180 / pi

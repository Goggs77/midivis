module Graphics.Gloss.Relative.Internal.Window where
  
import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as Gloss
import Graphics.Gloss.Relative.Internal.Dimension
import Graphics.Gloss.Relative.Internal.Picture
import Graphics.Gloss.Relative.Internal.Raster
import Graphics.Gloss.Relative.Internal.Data

import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Set (Set(..))
import qualified Data.Set as Set
  
-- * Window data type

-- | A function that returns which regions a position belongs to.
type RegionHandler = Point -> IO (Set String)

data WindowOutput = WindowOutput { wPic :: Picture, wHandler :: RegionHandler }

instance Semigroup WindowOutput where
    (WindowOutput p1 r1) <> (WindowOutput p2 r2) = WindowOutput (joinPicture p1 p2) (mappend r1 r2)
    
instance Monoid WindowOutput where
    mempty = WindowOutput emptyPicture mempty

newtype Window m a = Window { unWindow :: ReaderT Dimension (WriterT WindowOutput m) a }
    deriving (Monad,Applicative,Functor)
    
instance MonadTrans Window where
    lift m = Window $ lift $ lift m

instance MonadIO m => MonadIO (Window m) where
    liftIO = lift . liftIO

mkWindow :: Monad m => (Dimension -> WindowOutput) -> Window m ()
mkWindow f = Window $ do
    screen <- Reader.ask
    Writer.tell $ f screen

runWindow :: Monad m => Dimension -> Window m a -> m (a,WindowOutput)
runWindow screen (Window w) = Writer.runWriterT (Reader.runReaderT w screen)

execWindow :: Monad m => Dimension -> Window m a -> m WindowOutput
execWindow screen (Window w) = Writer.execWriterT (Reader.runReaderT w screen)

askDimension :: Monad m => Window m Dimension
askDimension = Window $ Reader.ask

withDimension :: Monad m => (Dimension -> Dimension) -> Window m a -> Window m a
withDimension f (Window w) = Window $ Reader.local f w

mapWindowOutput :: Monad m => (WindowOutput -> WindowOutput) -> Window m a -> Window m a
mapWindowOutput f (Window w) = Window $ Reader.mapReaderT (Writer.mapWriterT (\m -> m >>= \(x,y) -> return (x,f y))) w

emptyWindow :: Monad m => Window m ()
emptyWindow = tellWindowOutput mempty

tellWindowOutput :: Monad m => WindowOutput -> Window m ()
tellWindowOutput p = Window $ Writer.tell p

-- * Window transformations
  
-- | Builds a grid of windows, evenly splitting the screen both horiontally and vertically.
grid :: Monad m => [[Window m a]] -> Window m [[a]]
grid = rows . map columns

-- | Evenly splits the screen vertically into a list.
rows :: Monad m => [Window m a] -> Window m [a]
rows ws = Window $ do
    dim@(dimx,dimy) <- Reader.ask
    let dimyn = (realToFrac dimy) / (realToFrac $ length ws)
    let go [] = fmap (Prelude.const []) emptyWindow
        go (x:xs) = fmap (uncurry (:)) (top dimyn x (go xs))
    unWindow $ go ws

-- | Evenly splits the screen horizontally into a list.
columns :: Monad m => [Window m a] -> Window m [a]
columns ws = Window $ do
    dim@(dimx,dimy) <- Reader.ask
    let dimxn = (realToFrac dimx) / (realToFrac $ length ws)
    let go [] = fmap (Prelude.const []) emptyWindow
        go (x:xs) = fmap (uncurry (:)) (left dimxn x (go xs))
    unWindow $ go ws

-- | Top-biased vertical composition of two windows. Receives height of top row.
top :: Monad m => Float -> Window m a -> Window m b -> Window m (a,b)
top sy1 w1 w2  = Window $ do
    dim@(sx,sy) <- Reader.ask
    let sy2 = sy - sy1
    (a,WindowOutput p1 r1) <- lift $ lift $ runWindow (sx,sy1) w1
    (b,WindowOutput p2 r2) <- lift $ lift $ runWindow (sx,sy2) w2
    let p12 = Pictures [translateY (realToFrac sy2/2) p1,translateY (-realToFrac sy1/2) p2]
    let r12 pos = do
            m1 <- r1 $ translatePointY (-realToFrac sy2/2) pos
            m2 <- r2 $ translatePointY (realToFrac sy1/2) pos
            return $ Set.unions [m1,m2]
    Writer.tell (WindowOutput p12 r12)
    return (a,b)

-- | Bottom-biased vertical composition of two windows. Receives height of bottom row.
bottom :: Monad m => Float -> Window m a -> Window m b -> Window m (a,b)
bottom sy2 w1 w2 = do
    (w,h) <- askDimension
    top (h-sy2) w1 w2

-- Left-biased horizontal composition of two windows. Receives width of left column.
left :: Monad m => Float -> Window m a -> Window m b -> Window m (a,b)
left sx1 w1 w2 = Window $ do
    (sx,sy) <- Reader.ask
    let sx2 = sx - sx1
    (a,WindowOutput p1 r1) <- lift $ lift $ runWindow (sx1,sy) w1
    (b,WindowOutput p2 r2) <- lift $ lift $ runWindow (sx2,sy) w2
    let p12 = Pictures [translateX (-realToFrac sx2/2) p1,translateX (realToFrac sx1/2) p2]
    let r12 pos = do
            m1 <- r1 $ translatePointX (realToFrac sx2/2) pos
            m2 <- r2 $ translatePointX (-realToFrac sx1/2) pos
            return $ Set.unions [m1,m2]
    Writer.tell (WindowOutput p12 r12)
    return (a,b)

-- Right-biased horizontal composition of two windows. Receives width of right column.
right :: Monad m => Float -> Window m a -> Window m b -> Window m (a,b)
right sx2 w1 w2 = do
    (w,h) <- askDimension
    left (w-sx2) w1 w2

-- | Stretches a picture to fit the window
stretchWith :: Monad m => Dimension -> Picture -> Window m ()
stretchWith (cx,cy) pic = Window $ do
    screen@(sx,sy) <- Reader.ask
    let scalex = max 0 (realToFrac sx / realToFrac cx)
        scaley = max 0 (realToFrac sy / realToFrac cy)
    Writer.tell (WindowOutput (Scale scalex scaley pic) mempty)
    return ()

stretch :: Monad m => Picture -> Window m ()
stretch pic = stretchWith (pictureDimension pic) pic

alignWith :: Monad m => Dimension -> Alignment -> WindowOutput -> Window m ()
alignWith dim a (WindowOutput pic region) = Window $ do
    screen <- Reader.ask
    let (ax,ay) = alignCoords a dim screen
    let pic' = translateX ax $ translateY ay pic
    let region' pos = region $ translatePointX (-ax) $ translatePointY (-ay) pos
    Writer.tell (WindowOutput pic' region')

align :: Monad m => Alignment -> WindowOutput -> Window m ()
align a (WindowOutput pic region) = alignWith (pictureDimension pic) a (WindowOutput pic region)

fitWith :: Monad m => Dimension -> WindowOutput -> Window m ()
fitWith (cx,cy) (WindowOutput pic region) = Window $ do
    (sx,sy) <- Reader.ask
    let scalex = sx / cx
    let scaley = sy / cy
    let scalexy = max 0 (min scalex scaley)
    let dim' = (cx*scalexy,cy*scalexy)
    let pic' = Scale scalexy scalexy pic
    let region' pos = region $ scalePoint (1/scalexy) (1/scalexy) pos
    Writer.tell (WindowOutput pic' region')

fit :: Monad m => WindowOutput -> Window m ()
fit (WindowOutput pic region) = fitWith (pictureDimension pic) (WindowOutput pic region)

text :: Monad m => String -> Window m ()
text str = fitWith dim (WindowOutput pic mempty)
    where
    textWidth = realToFrac (length str) * (realToFrac charWidth)
    pic = Translate (-realToFrac textWidth/2) (-realToFrac charHeight/2) $ Text str
    dim = (textWidth,realToFrac charHeight)

-- * Regions

-- | Registers a new region for the current window, as a rectangle.
addRegionRectangle :: Monad m => String -> Window m ()
addRegionRectangle name = do
    dim <- askDimension
    let reg = dimensionToBox dim
    let react p = if pointInsideBox p reg
            then return $ Set.singleton name
            else return Set.empty
    tellWindowOutput $ WindowOutput emptyPicture react

-- | Registers a new region for the current window, pixel-wise. 
addRegionTransparent :: Monad m => String -> RasteredPicture -> Window m ()
addRegionTransparent name pic = do
    dim <- askDimension
    let reg = dimensionToBox dim
    let react p = case pointWithinBox p reg of
            Nothing -> return Set.empty
            Just p' -> getPixelAlpha pic p' >>= \alpha -> if alpha > 0
                then return $ Set.singleton name 
                else return $ Set.empty
    tellWindowOutput $ WindowOutput emptyPicture react

-- * Alignments

alignCoords :: Alignment -> Dimension -> Dimension -> (Float,Float)
alignCoords (Align ha va) c s = halignCoords ha c s Gloss.+ valignCoords va c s
alignCoords (AlignAt (ux,uy)) (cx,cy) (sx,sy) = (ax,ay)
    where
    x = resolveUnit sx ux
    y = resolveUnit sy uy
    hborder = sx / 2 - cx / 2
    vborder = sy / 2 - cy / 2
    ax = min hborder $ max (-hborder) x
    ay = min vborder $ max (-vborder) y
    
halignCoords :: HorizontalAlignment -> Dimension -> Dimension -> (Float,Float)
halignCoords a (cx,cy) (sx,sy) = case a of
    AlignLeft   -> (-((sx-cx)/2),0)
    AlignRight  -> (((sx-cx)/2),0)
    AlignCenter -> (0,0)

valignCoords :: VerticalAlignment -> Dimension -> Dimension -> (Float,Float)
valignCoords a (cx,cy) (sx,sy) = case a of
    AlignTop    -> (0,((sy-cy)/2))
    AlignBottom -> (0,-((sy-cy)/2))
    AlignMiddle -> (0,0)

largestAspectFitDim :: Float -> Float -> Dimension -> Dimension
largestAspectFitDim a b (screenW,screenH)
  | screenW / screenH >= a / b = (screenH * (a / b), screenH)
  | otherwise                  = (screenW, screenW * (b / a))
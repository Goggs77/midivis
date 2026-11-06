-- | Various game modes using frames instead of pictures.
module Graphics.Gloss.Relative.Interface
        ( module Graphics.Gloss.Data.Display
        , displayRelative, displayRelativeIO, playRelative, playRelativeIO, animateRelative
        , setWindowTitle, getWindowSize
        , isOver, mouseAt
        , Controller(..), Event(..), Mouse(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Relative.Frame
import Graphics.Gloss.Relative.Internal.Dimension
import qualified Graphics.Gloss.Relative.Internal.Window as Window
import qualified Graphics.Gloss.Relative.Internal.Frame as Frame
import qualified Graphics.Gloss.Relative.Internal.Raster as Raster
import qualified Graphics.Gloss.Relative.Internal.Cache as Cache
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import Graphics.Gloss.Interface.Pure.Game (Key(..), SpecialKey(..), MouseButton(..), KeyState(..), Modifiers(..))
import qualified Graphics.Gloss.Interface.IO.Display as Gloss
import Graphics.Gloss.Interface.IO.Display (Controller(..))
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
import qualified Graphics.Gloss.Interface.Environment as GlossEnv

import Control.Monad
import qualified Data.Set as Set
import Data.IORef
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit
import qualified Data.Text as T

-- | A custom Gloss event type that knows about frame regions.
data Event
    -- | A key press.
    = EventKey { eventKey :: Key, eventKeyState :: KeyState, eventModifiers :: Modifiers, eventMouse :: Mouse }
    -- | A mouse movement.
    | EventMotion { eventMouse :: Mouse }
    deriving (Eq, Show)

-- | Information about the mouse.
data Mouse = Mouse
    { mousePosition :: Point -- ^ Current mouse position.
    , mouseInside :: [String] -- ^ List of regions in which the mouse is currently inside. __Note:__ The exact regions correspond to the region names defined in the current 'Frame'.
    } deriving (Eq,Show)

-- | Whether the mouse is currently over the region with the given name.
isOver :: String -> Event -> Bool
isOver name = elem name . mouseInside . eventMouse

-- | The current mouse position carried by an event.
mouseAt :: Event -> Point
mouseAt = mousePosition . eventMouse

-- | Set the title of the currently open window at runtime.
--
--   Requires the window to already exist — call from a step, event or
--   controller callback, not before the display loop starts.
setWindowTitle :: T.Text -> IO ()
setWindowTitle = GlossEnv.setWindowTitle . T.unpack

-- | Get the size of the currently open window, in pixels @(width, height)@.
--
--   Requires the window to already exist — call from a step, event or
--   controller callback, not before the display loop starts.
getWindowSize :: IO (Int, Int)
getWindowSize = GlossEnv.getWindowSize

fromGlossEvent :: Gloss.Event -> Window.RegionHandler -> IO (Either Event Dimension)
fromGlossEvent (Gloss.EventMotion pos) region = do
    rs <- region pos
    return $ Left $ EventMotion (Mouse pos $ Set.toList rs)
fromGlossEvent (Gloss.EventKey k st m pos) region = do
    rs <- region pos
    return $ Left $ EventKey k st m (Mouse pos $ Set.toList rs)
fromGlossEvent (Gloss.EventResize screen) region = do
    return $ Right (screenSizeToDimension screen)

-- | A variant of 'Gloss.display' using 'Frame'.
displayRelative
    :: Display          -- ^ Display mode.
    -> Color            -- ^ Background color.
    -> Frame            -- ^ The frame to draw.
    -> IO ()
-- __Note:__ like 'playRelative', the frame is re-rendered when the window is resized.
displayRelative dis backColor frame =
    playRelative dis backColor 30 () (const frame) (\_ w -> w) (\_ w -> w)

-- | A variant of 'Gloss.displayIO' using 'Frame'.
displayRelativeIO
        :: Display                -- ^ Display mode.
        -> Color                  -- ^ Background color.
        -> IO Frame               -- ^ Action to produce the current frame.
        -> (Controller -> IO ())  -- ^ Callback to take the display controller.
        -> IO ()

displayRelativeIO dis backColor makeFrame eatController = do
    screen <- getDisplayDimension dis
    let makePicture = do
            frame <- makeFrame
            let pic = renderStaticFrame frame screen
            return pic
    Gloss.displayIO dis backColor makePicture eatController

-- | A variant of 'Gloss.animate' using 'Frame'. The frame is a function of the number of seconds since the start. The picture is automatically redimensioned on resize events.
animateRelative
    :: Display            -- ^ Display mode.
    -> Color              -- ^ Background color.
    -> (Float -> Frame)   -- ^ The frame to draw, as a function of the elapsed time in seconds.
    -> IO ()
animateRelative dis backColor mkFrame =
    playRelative dis backColor 60 0 mkFrame (\_ t -> t) (\dt t -> t + dt)

-- | A variant of 'Gloss.play' using 'Frame'. The resulting picture is automatically redimensioned on resize events.
playRelative
    :: Display              -- ^ Display mode.
    -> Color                -- ^ Background color.
    -> Int                  -- ^ Number of simulation steps to take for each second of real time.
    -> world                -- ^ The initial world.
    -> (world -> Frame)     -- ^ A function to convert the world a picture.
    -> (Event -> world -> world)
            -- ^ A function to handle input events.
    -> (Float -> world -> world)
            -- ^ A function to step the world one iteration.
            --   It is passed the period of time (in seconds) needing to be advanced.
    -> IO ()
playRelative display backColor simResolution worldStart worldToFrame worldHandleEvent worldAdvance = do
    let handleEvent (EventKey (Gloss.SpecialKey Gloss.KeyEsc) Gloss.Down _ _) st = exitSuccess
        handleEvent ev st = return $ worldHandleEvent ev st
    playRelativeIO display backColor simResolution worldStart (return . worldToFrame) handleEvent (\x -> return . worldAdvance x)

-- | A variant of 'Gloss.playIO' using 'Frame'. The resulting picture is automatically redimensioned on resize events.
playRelativeIO
    :: Display              -- ^ Display mode.
    -> Color                -- ^ Background color.
    -> Int                  -- ^ Number of simulation steps to take for each second of real time.
    -> world                -- ^ The initial world.
    -> (world -> IO Frame)  -- ^ A function to convert the world a picture.
    -> (Event -> world -> IO world)
            -- ^ A function to handle input events.
    -> (Float -> world -> IO world)
            -- ^ A function to step the world one iteration.
            --   It is passed the period of time (in seconds) needing to be advanced.
    -> IO ()
playRelativeIO display backColor simResolution worldStart worldToFrame worldHandleEvent worldAdvance = do
    handler :: IORef Window.RegionHandler <- newIORef mempty
    screen <- getDisplayScreenSize display
    let dim = screenSizeToDimension screen
    -- independent gloss state for parallel rendering
    glossState <- Gloss.initState
    cache <- Cache.newCacheTable
    currentFrame :: IORef Int <- newIORef 0 -- wraps around
    offscreen :: IORef (IO Raster.Offscreen) <- newIORef (Raster.createOffscreen screen)
    -- Render throttling: the simulation/input step stays at simResolution
    -- (1 kHz MIDI polling), but offscreen re-rendering is capped at 144 fps.
    -- Without the cap the draw callback would re-render 1000x/s, flooding the
    -- GHC heap and causing GC pauses that stall the audio callback thread.
    lastPicRef :: IORef Picture <- newIORef Blank
    lastRenderNs :: IORef Word64 <- newIORef 0
    -- pre-populate the region handler, so that mouse events arriving before the first frame is
    -- drawn already see the frame's regions (otherwise the first hover is silently lost)
    worldToFrame worldStart >>= \f0 ->
        writeIORef handler $! Window.wHandler (Frame.renderStaticFrameOutput f0 dim)
    let renderPeriodNs = 1000000000 `div` 144
        draw (w,_s) = do
            now <- getMonotonicTimeNSec
            lastNs <- readIORef lastRenderNs
            if now - lastNs < renderPeriodNs
                then readIORef lastPicRef
                else do
                    -- Use the CURRENT window size every frame instead of the
                    -- Resize-event-updated dim: the client (e.g. midivis)
                    -- reads getWindowSize directly, so using the event's s
                    -- here causes the two to diverge during a resize, which
                    -- breaks Aspect's aspect-ratio fit (stretch/crop).
                    ws <- GlossEnv.getWindowSize
                    let s = screenSizeToDimension ws
                    i <- readIORef currentFrame
                    f <- worldToFrame w
                    (Window.WindowOutput pic h) <- Frame.renderDynamicFrame i cache glossState offscreen f s
                    writeIORef handler $! h
                    writeIORef lastPicRef pic
                    writeIORef lastRenderNs now
                    writeIORef currentFrame $! i + 1
                    Cache.evictOldCacheTable (i+1) simResolution cache
                    return pic
    let handleEvent ev (w,s) = do
            h <- readIORef handler
            fromGlossEvent ev h >>= \e -> case e of
                Left ev' -> liftM (,s) (worldHandleEvent ev' w)
                Right s' -> return (w,s')
    let advance time (w,s) = liftM (,s) (worldAdvance time w)
    Gloss.playIO display backColor simResolution (worldStart,dim) draw handleEvent advance


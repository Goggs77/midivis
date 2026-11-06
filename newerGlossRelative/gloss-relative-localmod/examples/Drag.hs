-- | Drag — grab and drag a ball image with pixel-perfect hit-testing.
--
-- The ball is a PNG with an alpha channel wrapped in a 'preciseRegion', so only its
-- non-transparent pixels can be grabbed: clicking a transparent corner does nothing.
-- If the image cannot be loaded, it falls back to a vector 'solidEllipse'.
--
-- __Exercises:__ 'preciseRegion', 'alignAt', 'mouseAt', 'zoom' with 'Absolute' units, 'fit' (image), 'playRelative'.
module Main where

import Graphics.Gloss.Relative
import Graphics.Gloss.Juicy (loadJuicy)

data State = State { ballPosition :: Point, ballClick :: Bool }

initial :: State
initial = State (0,0) False

draw :: Maybe Picture -> State -> Frame
draw mbBall st = zoom (Absolute 120) (Absolute 120) (alignAt (Absolute x, Absolute y))
               $ preciseRegion "ball"
               $ ball
    where
    (x,y) = ballPosition st
    -- fall back to a vector ball when the image is unavailable
    ball = maybe (solidEllipse blue) fit mbBall

event :: Event -> State -> State
event e s = case e of
    EventKey { eventKey = MouseButton LeftButton, eventKeyState = kst } -> case kst of
        Down -> if isOver "ball" e then s { ballClick = True } else s
        Up   -> s { ballClick = False }
    EventMotion _ -> if ballClick s then s { ballPosition = mouseAt e } else s

advance :: Float -> State -> State
advance _ s = s

window :: Display
window = InWindow "gloss-relative: Drag" (800, 600) (10, 10)

main :: IO ()
main = do
    mbBall <- loadJuicy "examples/bola.png"
    case mbBall of
        Just _  -> return ()
        Nothing -> putStrLn "Could not load examples/bola.png (run from the project root); using a vector ball."
    playRelative window white 30 initial (draw mbBall) event advance

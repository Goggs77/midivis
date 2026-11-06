-- | Widgets — clickable buttons and a draggable ball in one interactive scene.
--
-- Two buttons are rectangular 'region's inside an overlaid panel; the ball is a pixel-perfect 'preciseRegion'.
--
-- __Exercises:__ 'region' vs 'preciseRegion', 'isOver', 'mouseAt', 'overlay', 'zoom', 'alignAt', 'background', 'bordered', 'playRelative'.
module Main where

import Graphics.Gloss.Relative

window :: Display
window = InWindow "gloss-relative: Widgets" (800, 600) (10, 10)

data State = State
    { dragging    :: Bool
    , ballPos     :: Point
    , lastClicked :: String
    }

initial :: State
initial = State False (120, 0) "none"

draw :: State -> Frame
draw st = overlay [ball, panel]
  where
    ball = zoom (Absolute 70) (Absolute 70) (alignAt (Absolute bx, Absolute by))
         $ preciseRegion "ball" (solidEllipse orange)
      where (bx, by) = ballPos st

    panel = zoom (Relative 0.3) (Relative 0.95) alignLeft
          $ background (greyN 0.9)
          $ inset (Relative 0.08)
          $ column
            [ button "reset"  (lastClicked st == "reset")
            , button "center" (lastClicked st == "center")
            , banner ("last: " ++ lastClicked st) (greyN 0.2)
            ]

    button name hot = inset (Relative 0.12)
        $ bordered (Absolute 4) black
        $ region name
        $ background (if hot then azure else greyN 0.75) (banner name black)

event :: Event -> State -> State
event e s = case e of
    EventKey { eventKey = MouseButton LeftButton, eventKeyState = Down }
        | isOver "ball"   e -> s { dragging = True }
        | isOver "reset"  e -> s { ballPos = (120, 0), lastClicked = "reset" }
        | isOver "center" e -> s { ballPos = (0, 0),   lastClicked = "center" }
        | otherwise         -> s
    EventKey { eventKey = MouseButton LeftButton, eventKeyState = Up } ->
        s { dragging = False }
    EventMotion _ -> if dragging s then s { ballPos = mouseAt e } else s
    _ -> s

advance :: Float -> State -> State
advance _ s = s

main :: IO ()
main = playRelative window white 30 initial draw event advance

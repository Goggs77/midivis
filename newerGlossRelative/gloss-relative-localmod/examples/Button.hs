-- | Button widget — a button that reacts to hover and press, over a textured image background.
--
-- The wood texture is a raw 'Picture' brought in with 'stretch' (the documented last-resort bridge
-- for image files); everything drawn on top of it is a native frame. If the image cannot be loaded,
-- it falls back to a plain background.
--
-- __Exercises:__ 'region', 'isOver', 'bordered', 'background', 'column', 'banner', 'stretch' (image), 'playRelative'.
module Main where

import Graphics.Gloss.Relative
import Graphics.Gloss.Juicy (loadJuicy)

data State = State { pressed :: Bool, selected :: Bool }

initial :: State
initial = State False False

draw :: Maybe Picture -> State -> Frame
draw mbWood st = overlay
    -- fall back to a plain background when the texture is unavailable
    [ maybe empty stretch mbWood
    , zoom (Relative 0.5) (Relative 0.5) alignCenter
        $ bordered (Absolute 10) borderc
        $ region "button"
        $ background fill
        $ column [banner "Press" labelc, banner "Hover" labelc]
    ]
    where
    -- hovering flips the fill dark -> light, and the labels stay legible either way
    fill    = if selected st then greyN 0.85 else greyN 0.25
    labelc  | pressed st  = red
            | selected st = black
            | otherwise   = white
    borderc = if pressed st then red else black

event :: Event -> State -> State
event e s = case e of
        EventKey { eventKey = MouseButton LeftButton, eventKeyState = kst } -> case kst of
            Down -> if isInside then s' { pressed = True } else s'
            Up   -> s' { pressed = False }
        _ -> s'
    where
    isInside = isOver "button" e
    s' = s { selected = isInside }

advance :: Float -> State -> State
advance _ s = s

window :: Display
window = InWindow "gloss-relative: Button" (800, 600) (10, 10)

main :: IO ()
main = do
    mbWood <- loadJuicy "examples/wood.png"
    case mbWood of
        Just _  -> return ()
        Nothing -> putStrLn "Could not load examples/wood.png (run from the project root); using a plain background."
    playRelative window white 30 initial (draw mbWood) event advance

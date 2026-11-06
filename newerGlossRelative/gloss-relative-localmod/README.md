
# Gloss Relative

This library is an extension to [Gloss](https://hackage.haskell.org/package/gloss) that removes the
pain of designing graphics with sizes relative to the screen dimensions. It is aimed at beginners as a
minimalistic, expressive replacement for plain Gloss.

Instead of Gloss's absolute-pixel `Picture`, you describe a `Frame`: a picture whose size and position
are given *relative to the frame it lives in*. Frames scale themselves to any window, resize
automatically, lay out in grids/rows/columns, and support native mouse hover events over named regions.

## Usage

This package is a drop-in, batteries-included replacement for the standard:

```haskell
import Graphics.Gloss
```

Just replace it with the new import to get the Gloss picture-building vocabulary plus the `Frame`
abstraction:

```haskell
import Graphics.Gloss.Relative
```

The whole Gloss picture vocabulary (`Picture`, `Color` and its constants, points and vectors, and all the
shape builders) is re-exported. Gloss's own entry points (`display`, `play`, `animate`, `simulate`) are
*replaced* by the frame-based `displayRelative`, `playRelative` and `animateRelative`, so there is no name
clash — the relative interface subsumes Gloss's.

### Units

Anything sizeable takes a `Unit`, so you can freely mix screen-relative fractions and absolute pixels:

```haskell
zoom (Relative 0.5) (Relative 0.5) alignCenter frame  -- half of the frame
zoom (Absolute 150) (Absolute 150) alignCenter frame  -- exactly 150x150 pixels
```

## Examples

You can play with a few examples that illustrate how the library can be used:

```
cabal run gloss-relative-gallery   # static: every native drawing primitive
cabal run gloss-relative-checkers  # a resolution-independent checkers board
cabal run gloss-relative-units     # Relative vs Absolute units (resize the window!)
cabal run gloss-relative-clock     # an animated clock, no raw Picture used
cabal run gloss-relative-button    # a hoverable, pressable button
cabal run gloss-relative-drag      # drag a ball with pixel-perfect hit-testing
cabal run gloss-relative-widgets   # buttons + dragging together, an interactive scene
```

The `button` and `drag` examples load images from `examples/`, so run them from the project root.
Press `Esc` to close any example.

-- | Runtime rendering of UTF-8 text into gloss 'Picture's and
--   gloss-relative 'Frame's, using the compile-time glyph cache from
--   "Midivis.Resources.Font".
--
--   The glyph textures are white-with-alpha; because gloss's bitmap
--   rendering forces the current color to white, colored text is obtained
--   by re-baking atlas pages tinted with the requested color.
--
--   IMPORTANT (performance): 'BitmapData' values are cached per
--   (page, color) in an 'IORef'.  gloss's texture cache keys on the
--   stable name of the 'BitmapData', so reusing the same value avoids
--   re-uploading the multi-MB atlas pages every frame.
module Midivis.Render.Text
    ( utf8Picture
    , utf8PictureColored
      -- * Sized text frames
    , TextSize(..)
    , titleSize, subtitleSize, bodySize
    , TextFill(..)
    , utf8Banner
      -- * Wrapped multi-line text box
    , WrapMethod(..)
    , LineWidthMode(..)
    , TextDebug(..)
    , utf8TextBox
    , wrapLines
    ) where

import           Data.Bits                ( shiftL, (.|.) )
import           Data.IORef
import           Data.IntMap.Strict       ( IntMap )
import qualified Data.IntMap.Strict       as IM
import qualified Data.Map.Strict          as M
import           Data.Maybe               ( fromMaybe )
import           System.IO.Unsafe         ( unsafePerformIO )

import           Graphics.Gloss.Data.Bitmap
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Relative   ( Frame(..), Alignment(..), Unit(..)
                                           , HorizontalAlignment(..)
                                           , VerticalAlignment(..) )

import           Midivis.Resources.Font   ( GlyphInfo(..), AtlasPage(..)
                                          , FontMetrics(..)
                                          , fontGlyphMap, fontAtlas, fontMetrics
                                          , fontKerning, missingGlyph )

-- | The raster size of the glyph atlas (px per em).  The atlas is built at
--   this size; text at other sizes is obtained by uniform scaling.
rasterSize :: Float
rasterSize = 64

-------------------------------------------------------------------------------
-- Atlas pages -> gloss BitmapData (cached per page & color)
-------------------------------------------------------------------------------

-- | Cache the single white/coverage atlas page for each page index.  Color is
--   applied at draw time, so changing text color no longer duplicates a
--   multi-megabyte page in CPU and GPU memory.
atlasCache :: IORef (IntMap BitmapData)
atlasCache = unsafePerformIO (newIORef IM.empty)
{-# NOINLINE atlasCache #-}

-- | Get (or build-and-cache) the white coverage bitmap for atlas page @i@.
atlasData :: Int -> BitmapData
atlasData i = unsafePerformIO $ do
    pages <- readIORef atlasCache
    case IM.lookup i pages of
      Just bd -> pure bd
      Nothing -> do
        let bd = whitePage i
        modifyIORef' atlasCache (IM.insert i bd)
        pure bd

-- | Integer key of a color (RGBA 8-bit packed) for the cache.
colorKey :: Color -> Int
colorKey c = (w r `shiftL` 24) .|. (w g `shiftL` 16) .|. (w b `shiftL` 8) .|. w a
  where
    (r, g, b, a) = rgbaOfColor c
    w x = fromIntegral (round (x * 255)) :: Int

-- | Untouched white page: RGBA, top-to-bottom.
whitePage :: Int -> BitmapData
whitePage i =
    bitmapDataOfTintableByteString w h fmt (apBytes p) True
  where
    p        = fontAtlas !! i
    (w, h)   = apSize p
    fmt      = BitmapFormat TopToBottom PxRGBA

-------------------------------------------------------------------------------
-- Glyph layout (shared by single-line and multi-line text)
-------------------------------------------------------------------------------

lookupGlyph :: Char -> GlyphInfo
lookupGlyph c' = IM.findWithDefault (missingGlyph c') (fromEnum c') fontGlyphMap

-- | Place a line of text: list of (pen x, glyph).  The baseline is at
--   y = 0; pens advance by (fractional) advance + kerning pair.
placeLine :: String -> [(Float, GlyphInfo)]
placeLine str = go 0 (map lookupGlyph str)
  where
    go _ [] = []
    go pen (gi : rest) =
        (pen, gi) : go (pen + gAdvance gi + kern gi rest) rest
    kern gi (gi' : _) = kernPx gi gi'
    kern _ []         = 0

-- | Kerning adjustment between two adjacent glyphs (pixels).
kernPx :: GlyphInfo -> GlyphInfo -> Float
kernPx gi gi' =
    fromIntegral $ fromMaybe 0 $ do
        km <- IM.lookup (gChar gi) fontKerning
        IM.lookup (gChar gi') km

-- | Width of a line at the raster size: the right edge of the last glyph
--   (pen + advance).  Zero for an empty line.
lineWidth :: String -> Float
lineWidth str = case placeLine str of
    [] -> 0
    xs -> let (pen, gi) = last xs in pen + gAdvance gi

-- | Vertical distance between baselines (ascender minus descender).
lineHeight :: Float
lineHeight =
    fromIntegral (fmAscender fontMetrics)
    - fromIntegral (fmDescender fontMetrics)

-- | FreeType placement: centre of the glyph bitmap given its pen x.
glyphCentre :: Float -> GlyphInfo -> (Float, Float)
glyphCentre pen gi =
    ( pen + fromIntegral bx + fromIntegral w / 2
    , fromIntegral by - fromIntegral h / 2 )
  where
    (w, h)  = gSize gi
    (bx, by) = gBearing gi

-- | Picture of one glyph at the given pen position (baseline y = 0).
glyphPic :: Color -> Float -> GlyphInfo -> Picture
glyphPic c pen gi
    | w <= 0 || h <= 0 = mempty
    | otherwise =
        Color c $
            Translate px py
                (bitmapSection (Rectangle (gRect gi) (gSize gi))
                               (atlasData (pageOf gi)))
  where
    (w, h)  = gSize gi
    (px, py) = glyphCentre pen gi

-- | Picture of a single line of text (baseline at y = 0, origin at the
--   first glyph's pen).
linePicture :: Color -> String -> Picture
linePicture c str =
    Pictures [ glyphPic c pen gi | (pen, gi) <- placeLine str ]

-- | Per-glyph bounding boxes of a line, offset by @x0@ (baseline y = 0).
lineGlyphBoxes :: Float -> String -> [(Float, Float, Float, Float)]
lineGlyphBoxes x0 str =
    [ let (px, py) = glyphCentre (x0 + pen) gi
      in ( px - fromIntegral w / 2, py - fromIntegral h / 2
         , px + fromIntegral w / 2, py + fromIntegral h / 2 )
    | (pen, gi) <- placeLine str
    , let (w, h) = gSize gi
    , w > 0, h > 0 ]

-------------------------------------------------------------------------------
-- Text -> Picture (single line)
-------------------------------------------------------------------------------

-- | Render a string as a white-text 'Picture' using the font atlas.
utf8Picture :: String -> Picture
utf8Picture = utf8PictureColored white

-- | Render a string as a 'Picture' tinted with a color.
--
--   The returned picture has its bounding box centred on the origin:
--   gloss-relative's 'stretch'/'fit' compute the picture dimension via
--   'boxScreenSize', which assumes the bbox is symmetric about the origin
--   (@width = max(|left|,|right|) * 2@).  We therefore compute the union
--   bbox and translate the whole text by its centre.
--
--   Memoised: texts are frame-stable (names, tunings, chord labels), so the
--   whole Picture tree plus its bbox is built once per (color, text) and
--   reused — only the glyph-atlas texture cache existed before.
utf8PictureColored :: Color -> String -> Picture
utf8PictureColored c str = fst (memoText c str)

-- | Pixel size of the text picture at the raster size (bounding box).
--   Returns (0,0) for empty/invisible text.
textRasterSize :: Color -> String -> (Float, Float)
textRasterSize c str = snd (memoText c str)

-- | Memoised single-line text: (Picture, bbox size) keyed by (color, text).
textMemoCache :: IORef (M.Map (Int, String) (Picture, (Float, Float)))
textMemoCache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE textMemoCache #-}

-- | Bound the cache size.  On overflow, evict one deterministic entry rather
--   than discarding the entire useful cache.
--   @capped n k v = \\m -> ...@ so it can be used with 'modifyIORef''.
capped :: Ord k => Int -> k -> v -> M.Map k v -> M.Map k v
capped n k v m
    | M.member k m = m'
    | M.size m < n = m'
    | otherwise    = M.insert k v (snd (M.deleteFindMin m))
  where
    m' = M.insert k v m

memoText :: Color -> String -> (Picture, (Float, Float))
memoText c str = unsafePerformIO $ do
    let k = (colorKey c, str)
    m <- readIORef textMemoCache
    case M.lookup k m of
        Just v  -> pure v
        Nothing -> do
            let v = (pic, sz)
            modifyIORef' textMemoCache (capped 1024 k v)
            pure v
  where
    pic = Translate (-cx) (-cy) (linePicture c str)
    sz  = (xmax - xmin, ymax - ymin)
    boxes = lineGlyphBoxes 0 str
    (cx, cy) = bboxCentre boxes
    (xmin, ymin, xmax, ymax) = bboxOf boxes

-- | Bounding box (minx, miny, maxx, maxy) of a list of boxes.
bboxOf :: [(Float, Float, Float, Float)] -> (Float, Float, Float, Float)
bboxOf boxes =
    case foldr grow Nothing boxes of
      Nothing -> (0, 0, 0, 0)
      Just b  -> b
  where
    grow :: (Float, Float, Float, Float) -> Maybe (Float, Float, Float, Float)
           -> Maybe (Float, Float, Float, Float)
    grow (xmin, ymin, xmax, ymax) Nothing =
        Just (xmin, ymin, xmax, ymax)
    grow (xmin, ymin, xmax, ymax) (Just (a, b, c, d)) =
        Just (min xmin a, min ymin b, max xmax c, max ymax d)

-- | Centre of the union of axis-aligned boxes.
--   Degenerates to the origin when there are no visible glyphs.
bboxCentre :: [(Float, Float, Float, Float)] -> (Float, Float)
bboxCentre boxes =
    case foldr grow Nothing boxes of
      Nothing -> (0, 0)
      Just (xmin, ymin, xmax, ymax) ->
          ((xmin + xmax) / 2, (ymin + ymax) / 2)
  where
    grow :: (Float, Float, Float, Float) -> Maybe (Float, Float, Float, Float)
           -> Maybe (Float, Float, Float, Float)
    grow (xmin, ymin, xmax, ymax) Nothing =
        Just (xmin, ymin, xmax, ymax)
    grow (xmin, ymin, xmax, ymax) (Just (a, b, c, d)) =
        Just (min xmin a, min ymin b, max xmax c, max ymax d)

pageOf :: GlyphInfo -> Int
pageOf = gPage

-------------------------------------------------------------------------------
-- Wrapping (multi-line layout)
-------------------------------------------------------------------------------

-- | How a text box wraps long lines.
data WrapMethod
    = NoWrap      -- ^ No automatic wrapping: only explicit @'\\n'@ breaks.
    | WrapByChar  -- ^ Break at any character when the line exceeds the width.
    | WrapByWord  -- ^ Break at word boundaries (whitespace).
    deriving (Show, Eq, Ord)

-- | How the line-width parameter of 'utf8TextBox' is interpreted.
--
--   The purpose is to keep the text box's layout size stable when the
--   content changes (so that, under 'FillFit', the font size does not jump
--   as the number of lines changes) — it does *not* limit the number of
--   characters per line.
data LineWidthMode
    = MaxLineWidth
        -- ^ The 'Float' is an upper bound: lines wrap when they exceed it.
        --   The box width is the wider of (content widest line, bound).
        --   Lines never exceed the bound, but the box can be narrower when
        --   all lines are short.
    | FixedFrameWidth
        -- ^ The 'Float' is a constant frame width: the text box itself is
        --   laid out at exactly this width (the alignment reference AND
        --   the box dimension are constant, so under 'FillFit' the font
        --   scale stays put as lines are added/removed).  Content wider
        --   than the bound still wraps according to 'WrapMethod'.
    | AdaptiveLineWidth
        -- ^ The 'Float' is ignored; the box width adapts to the widest
        --   line of the content (fully content-driven).
    deriving (Show, Eq, Ord)

-- | Debug overlays for 'utf8TextBox'.
data TextDebug
    = DebugNone    -- ^ No overlays.
    | DebugBox     -- ^ Outline of the whole text block (its own bbox).
    | DebugGlyphs  -- ^ Outline of every laid-out glyph bbox.
    | DebugAll     -- ^ Both 'DebugBox' and 'DebugGlyphs'.
    deriving (Show, Eq, Ord)

-- | Split a string into lines: explicit @'\\n'@ breaks are preserved as
--   separate (possibly empty) lines, then each line is soft-wrapped
--   according to the 'WrapMethod' with the given maximum width (pixels at
--   the raster size).
wrapLines :: WrapMethod -> Float -> String -> [String]
wrapLines wm maxW txt = concatMap (wrapLine wm maxW) (hardLines txt)
  where
    hardLines :: String -> [String]
    hardLines = foldr step [[]]
      where
        step '\n' acc = [] : acc
        step c (x : xs) = (c : x) : xs
        step _ [] = [[]]

-- | Wrap a single (already newline-free) line.
wrapLine :: WrapMethod -> Float -> String -> [String]
wrapLine _ _ [] = [""]                  -- keep empty line (occupies a row)
wrapLine NoWrap _ l = [l]
wrapLine WrapByChar maxW l = wrapByChar maxW l
wrapLine WrapByWord maxW l = wrapByWord maxW l

-- | Greedy char-wise wrap.  Each output line takes as many characters as
--   fit within @maxW@ (at least one).
wrapByChar :: Float -> String -> [String]
wrapByChar maxW str
    | maxW <= 0 = [str]
    | otherwise = go str
  where
    go [] = []
    go s =
        let (line, rest) = takeLine s
        in line : go rest
    -- right edge of each glyph = pen + advance; first one to exceed maxW
    -- ends the line.
    takeLine s =
        let over = [ (i, pen + gAdvance gi)
                   | (i, (pen, gi)) <- zip [0 ..] (placeLine s)
                   , pen + gAdvance gi > maxW ]
        in case over of
             []           -> (s, [])
             ((0, _) : _) -> (take 1 s, drop 1 s)
             ((i, _) : _) -> (take i s, drop i s)

-- | Greedy word-wise wrap.  Words are separated by the ASCII space
--   U+0020 only; other Unicode spaces (NBSP, EM SPACE, ...) stay inside
--   the word and never trigger a line break.
wrapByWord :: Float -> String -> [String]
wrapByWord maxW str
    | maxW <= 0 = [str]
    | otherwise = go (splitWords str)
  where
    go [] = []
    go (w : ws) =
        let (line, rest) = takeLine [w] (lineWidth w) ws
        in joinWords line : go rest
    takeLine acc accW [] = (acc, [])
    takeLine acc accW (w : ws)
        | accW + spaceW + lineWidth w > maxW = (acc, w : ws)
        | otherwise = takeLine (acc ++ [w]) (accW + spaceW + lineWidth w) ws
    spaceW = lineWidth " "

-- | Split on the ASCII space U+0020 only; runs of spaces collapse into
--   one separator.  All other Unicode whitespace is kept inside a word
--   (so it never breaks a line).
splitWords :: String -> [String]
splitWords = filter (not . null) . foldr step [[]]
  where
    step ' ' (w : ws) = [] : w : ws
    step c   (w : ws) = (c : w) : ws
    step _   []       = [[]]

-- | Join words with a single ASCII space.
joinWords :: [String] -> String
joinWords = unwords

-- | Horizontal alignment of a text box's lines, derived from the
--   'Alignment''s horizontal component.
lineAlignOf :: Alignment -> HorizontalAlignment
lineAlignOf (Align h _) = h
lineAlignOf (AlignAt _) = AlignCenter

-- | Vertical alignment of a text box, derived from the 'Alignment''s
--   vertical component.
alignVOf :: Alignment -> VerticalAlignment
alignVOf (Align _ v) = v
alignVOf (AlignAt _) = AlignMiddle

-- | Starting x of a line of width @w@ inside the box width @maxW@.
lineStartX :: HorizontalAlignment -> Float -> Float -> Float
lineStartX AlignLeft   _    _ = 0
lineStartX AlignCenter maxW w = (maxW - w) / 2
lineStartX AlignRight  maxW w = maxW - w

-- | Lay out a list of lines into one picture centred on the box centre.
--   @boxW@ is the reference width used for per-line alignment AND the
--   returned dimension's width; it is provided by the caller so that it
--   can stay constant across content changes ('FixedFrameWidth').
--
--   Layout coordinates: the box spans @[0,boxW] × [-boxH,0]@ (first line
--   baseline at y = 0).  The whole block is translated so the BOX centre
--   lands on the origin — not the glyph bbox centre — so per-line
--   horizontal alignment (left/centre/right via 'lineStartX') is
--   preserved relative to the box edges.
--   Returns the picture and its pixel size (boxW, boxH).
textBlock :: Alignment -> TextDebug -> Color -> [String] -> Float
          -> (Picture, (Float, Float))
textBlock align dbg c lines_ boxW =
    ( Translate (-boxW / 2) (boxH / 2) (Pictures (contentPics ++ dbgPics'))
    , (boxW, boxH) )
  where
    lineH = lineHeight
    rows  = zip [0 ..] lines_
    x0s   = [ lineStartX (lineAlignOf align) boxW (lineWidth ln) | ln <- lines_ ]
    -- box height: full line grid (all rows, incl. empty ones)
    boxH  = fromIntegral (length lines_) * lineH

    -- one picture per row: row i baseline at y = -i*lineH
    contentPics =
        [ Translate x0 (-fromIntegral i * lineH) (linePicture c ln)
        | (i, ln) <- rows
        , let x0 = x0s !! i ]

    -- all glyph boxes in the laid-out coordinate system
    allBoxes =
        concat
            [ [ let (px, py) = glyphCentre (x0 + pen) gi
                in ( px - fromIntegral w / 2
                   , py - yOff - fromIntegral h / 2
                   , px + fromIntegral w / 2
                   , py - yOff + fromIntegral h / 2 )
              | (pen, gi) <- placeLine ln
              , let (w, h) = gSize gi
              , w > 0, h > 0 ]
            | (i, ln) <- rows
            , let x0  = x0s !! i
                  yOff = fromIntegral i * lineH ]

    -- Debug boxes live in the SAME coordinate system as the glyphs
    -- (laid-out coordinates); the outer 'Translate' centres everything
    -- together, so the boxes align exactly with the text.
    -- 'DebugBox' outlines the text-box frame itself (boxW × boxH).
    dbgPics' = case dbg of
        DebugNone   -> []
        DebugBox    -> [ boxWire (0, -boxH, boxW, 0) yellow ]
        DebugGlyphs -> [ boxWire (a, b, c, d) red
                       | (a, b, c, d) <- allBoxes ]
        DebugAll    ->
            [ boxWire (0, -boxH, boxW, 0) yellow ]
            ++ [ boxWire (a, b, c, d) red
               | (a, b, c, d) <- allBoxes ]

-- | Wireframe rectangle of an axis-aligned box.
boxWire :: (Float, Float, Float, Float) -> Color -> Picture
boxWire (xmin, ymin, xmax, ymax) col =
    Translate ((xmin + xmax) / 2) ((ymin + ymax) / 2)
              (Color col (rectangleWire (xmax - xmin) (ymax - ymin)))

-------------------------------------------------------------------------------
-- Sized text frames
-------------------------------------------------------------------------------

-- | Desired font size in pixels (per em).  The glyph atlas is rasterized at
--   'rasterSize' px, so smaller/larger sizes are uniform scalings of it.
newtype TextSize = TextSize { textSizePx :: Float }
    deriving (Show, Eq, Ord)

-- | Heading size (large).
titleSize :: TextSize
titleSize = TextSize 64

-- | Sub-heading size (medium).
subtitleSize :: TextSize
subtitleSize = TextSize 40

-- | Body text size (small).
bodySize :: TextSize
bodySize = TextSize 24

-- | How a text picture is fitted into its frame.
data TextFill
    = FillFit      -- ^ Fit inside the frame preserving aspect ratio
                   --   (default): like gloss-relative's 'fit', but with a
                   --   configurable 'Alignment' instead of hard-coded center.
    | FillStretch  -- ^ Non-uniformly stretch to fill the whole frame.
    | FillNative   -- ^ Draw at the exact pixel size of the requested
                   --   'TextSize', no scaling to the frame.
    deriving (Show, Eq, Ord)

-- | Wrap a laid-out picture into a sized frame.
--   For single-line text ('utf8Banner') 'FillFit' preserves the aspect
--   ratio (line count never changes, so the scale is stable).
frameFromPicture :: TextSize -> TextFill -> Alignment -> Picture
                 -> (Float, Float) -> Frame
frameFromPicture (TextSize sz) fill align pic0 (tw0, th0) =
    case fill of
      FillFit     -> Aspect textDim align (Stretch (Just textDim) pic)
      FillStretch -> Stretch Nothing pic
      FillNative  -> Sized $ \dim -> Stretch (Just dim) pic
  where
    pic     = Scale k k pic0
    k       = sz / rasterSize
    textDim = (tw0 * k, th0 * k)

-- | Wrap a multi-line text-box picture into a sized frame.
--
--   'FillFit' never scales the text ABOVE its 'TextSize' natural size:
--   @s = min 1 (frameW / boxW) (frameH / boxH)@.  When the text fits, the
--   font size equals 'TextSize' exactly and stays constant as the number
--   of lines changes (so 'FixedFrameWidth' boxes keep a stable font);
--   when the box is smaller than the frame, the whole block shrinks
--   uniformly to fit (no overflow).  Vertical placement follows the
--   'Alignment''s vertical component.
textBoxFrameFromPicture :: TextSize -> TextFill -> Alignment -> Picture
                        -> (Float, Float) -> Frame
textBoxFrameFromPicture (TextSize sz) fill align pic0 (tw0, th0) =
    case fill of
      FillFit     -> Sized $ \(fw, fh) ->
                        let s  = minimum [ 1
                                         , fw / max 1 (tw0 * k)
                                         , fh / max 1 (th0 * k) ]
                            pw = tw0 * k * s
                            ph = th0 * k * s
                            dx = hShift align pw fw
                            dy = vShift align ph fh
                        in Stretch (Just (fw, fh))
                                  (Translate dx dy (Scale s s pic))
      FillStretch -> Stretch Nothing pic
      FillNative  -> Sized $ \(fw, fh) ->
                        let pw = tw0 * k
                            ph = th0 * k
                            dx = hShift align pw fw
                            dy = vShift align ph fh
                        in Stretch (Just (fw, fh))
                                  (Translate dx dy pic)
  where
    pic = Scale k k pic0
    k   = sz / rasterSize
    -- horizontal placement of a box of width @pw@ inside width @fw@:
    -- the box's own edge aligns to the parent frame's edge.
    hShift :: Alignment -> Float -> Float -> Float
    hShift a pw fw = case lineAlignOf a of
        AlignLeft   -> -(fw - pw) / 2
        AlignCenter -> 0
        AlignRight  ->  (fw - pw) / 2
    -- vertical placement of a picture of height @ph@ inside height @fh@
    vShift :: Alignment -> Float -> Float -> Float
    vShift a ph fh = case alignVOf a of
        AlignTop    -> (fh - ph) / 2
        AlignMiddle -> 0
        AlignBottom -> -(fh - ph) / 2

-- | Render UTF-8 text as a gloss-relative 'Frame' at the given size,
--   fitted according to 'TextFill' and aligned with 'Alignment'.
--
--   See 'utf8TextBox' for the multi-line variant.
utf8Banner :: TextSize -> TextFill -> Alignment -> String -> Color -> Frame
utf8Banner sz fill align txt c =
    frameFromPicture sz fill align (utf8PictureColored c txt)
                      (textRasterSize c txt)

-- | Render UTF-8 text as a multi-line text box.
--
--   * @WrapMethod@ controls wrapping: 'NoWrap' honours only explicit
--     @'\\n'@ breaks; 'WrapByChar'/'WrapByWord' additionally break long
--     lines.  'WrapByWord' breaks at the ASCII space only — Unicode
--     spaces (NBSP, EM SPACE, ...) stay inside the word and never break.
--   * @LineWidthMode@ decides how the following 'Float' is used (max line
--     width, constant frame width, or ignored for adaptive width) — see
--     'LineWidthMode'.
--   * The @Alignment@'s horizontal component aligns each line within the
--     box width (left/centre/right); its vertical component plus
--     'TextFill' place the whole block inside the frame.
--   * 'TextDebug' overlays the block's own bounding box ('DebugBox') and
--     the per-glyph bounding boxes ('DebugGlyphs').
utf8TextBox :: TextSize -> WrapMethod -> LineWidthMode -> Float
            -> TextFill -> Alignment -> TextDebug -> String -> Color -> Frame
utf8TextBox sz wm mode bound fill align dbg txt c =
    textBoxFrameFromPicture sz fill align pic0 dim
  where
    (pic0, dim) = memoTextBox wm mode bound align dbg c txt

-- | Key of the multi-line layout memo: every layout-affecting parameter.
data BoxKey = BoxKey
    { bkWrap  :: WrapMethod
    , bkMode  :: LineWidthMode
    , bkBound :: Float
    , bkAlign :: Alignment
    , bkDbg   :: TextDebug
    , bkColor :: Int
    , bkText  :: String
    } deriving (Eq, Ord, Show)

-- | Memoised multi-line layout: (Picture, box size) keyed by all layout
--   parameters.  The chord-name box only changes at note events, not per
--   frame, so the wrap + glyph placement is computed once per label.
boxMemoCache :: IORef (M.Map BoxKey (Picture, (Float, Float)))
boxMemoCache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE boxMemoCache #-}

memoTextBox :: WrapMethod -> LineWidthMode -> Float -> Alignment -> TextDebug
            -> Color -> String -> (Picture, (Float, Float))
memoTextBox wm mode bound align dbg c txt = unsafePerformIO $ do
    let k = BoxKey wm mode bound align dbg (colorKey c) txt
    m <- readIORef boxMemoCache
    case M.lookup k m of
        Just v  -> pure v
        Nothing -> do
            let v = layoutBox
            modifyIORef' boxMemoCache (capped 1024 k v)
            pure v
  where
    layoutBox = textBlock align dbg c lines_ boxW
    lines_ = wrapLines wm bound txt
    widest = maximum (0 : map lineWidth lines_)
    -- box width = the frame width the text block claims.
    -- 'FixedFrameWidth' makes it constant so FillFit's scale is stable
    -- as the number of lines changes.
    boxW = case mode of
        MaxLineWidth      -> max bound widest
        FixedFrameWidth   -> bound
        AdaptiveLineWidth -> widest

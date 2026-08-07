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
    ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Lazy     as BL
import           Data.Bits                ( shiftL, (.|.) )
import           Data.IORef
import           Data.IntMap.Strict       ( IntMap )
import qualified Data.IntMap.Strict       as IM
import           Data.Word                ( Word8 )
import           Data.Maybe               ( fromMaybe )
import           System.IO.Unsafe         ( unsafePerformIO )

import           Graphics.Gloss.Data.Bitmap
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Relative   ( Frame(..), Alignment
                                           , Unit(..) )

import           Midivis.Resources.Font   ( GlyphInfo(..), AtlasPage(..)
                                          , fontGlyphMap, fontAtlas
                                          , fontKerning, missingGlyph )

-- | The raster size of the glyph atlas (px per em).  The atlas is built at
--   this size; text at other sizes is obtained by uniform scaling.
rasterSize :: Float
rasterSize = 64

-------------------------------------------------------------------------------
-- Atlas pages -> gloss BitmapData (cached per page & color)
-------------------------------------------------------------------------------

-- | Cache: page index -> (colorKey -> BitmapData).
--   colorKey of white is used for the untouched white pages.
atlasCache :: IORef (IntMap (IntMap BitmapData))
atlasCache = unsafePerformIO (newIORef IM.empty)
{-# NOINLINE atlasCache #-}

whiteKey :: Int
whiteKey = colorKey white

-- | Get (or build-and-cache) the 'BitmapData' of atlas page @i@ tinted with
--   color @c@.  White is short-circuited to the original white page (no
--   per-pixel baking), so the common case costs nothing.
atlasData :: Color -> Int -> BitmapData
atlasData c i = unsafePerformIO $ do
    pages <- readIORef atlasCache
    let k = colorKey c
    case IM.lookup i pages >>= IM.lookup k of
      Just bd -> pure bd
      Nothing -> do
        let bd | k == whiteKey = whitePage i
               | otherwise     = tintedPage c i
        modifyIORef' atlasCache
            (IM.insertWith IM.union i (IM.singleton k bd))
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
    bitmapDataOfByteString w h fmt (apBytes p) True
  where
    p        = fontAtlas !! i
    (w, h)   = apSize p
    fmt      = BitmapFormat TopToBottom PxRGBA

-- | Tinted copy of a page: RGB scaled by color, alpha scaled by color alpha.
--   Uses a 'Data.ByteString.Builder' for a single-pass O(size) build (no
--   [Word8] list buildup — that would blow the -M256M heap on multi-MB pages).
tintedPage :: Color -> Int -> BitmapData
tintedPage c i =
    bitmapDataOfByteString w h fmt bytes' True
  where
    p         = fontAtlas !! i
    (w, h)    = apSize p
    fmt       = BitmapFormat TopToBottom PxRGBA
    (cr, cg, cb, ca) = rgbaOfColor c
    src       = apBytes p
    n         = BS.length src
    bytes'    = BL.toStrict (BB.toLazyByteString (go 0))
      where
        go :: Int -> BB.Builder
        go k
            | k >= n    = mempty
            | otherwise =
                BB.word8 (scale8 cr r) <> BB.word8 (scale8 cg g)
                <> BB.word8 (scale8 cb b) <> BB.word8 (scale8 ca a)
                <> go (k + 4)
          where
            r = BS.index src k
            g = BS.index src (k + 1)
            b = BS.index src (k + 2)
            a = BS.index src (k + 3)
    scale8 :: Float -> Word8 -> Word8
    scale8 f w = fromIntegral (round (fromIntegral w * f) `min` 255)

-------------------------------------------------------------------------------
-- Text -> Picture
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
--   Glyph placement follows the FreeType convention (baseline at y = 0):
--   the glyph bitmap's top-left corner sits at @(pen + bitmap_left,
--   -bitmap_top)@, so its centre is @(pen + left + w/2, top - h/2)@.
--   The pen advances by the (fractional) advance plus any kerning pair
--   adjustment; all glyphs share the same baseline, so they neither
--   overlap, gap nor jump vertically.
utf8PictureColored :: Color -> String -> Picture
utf8PictureColored c str =
    Translate (-cx) (-cy) $ Pictures $ zipWith step pens gs
  where
    gs   = map lookupGlyph str
    pens = penPositions gs

    -- per-glyph bbox in the laid-out coordinate system
    glyphBoxes :: [(Float, Float, Float, Float)]  -- (xmin,ymin,xmax,ymax)
    glyphBoxes =
        [ let (px, py) = glyphCentre x gi
          in ( px - fromIntegral w / 2, py - fromIntegral h / 2
             , px + fromIntegral w / 2, py + fromIntegral h / 2 )
        | (x, gi) <- zip pens gs
        , let (w, h) = gSize gi
        , w > 0, h > 0 ]

    (cx, cy) = bboxCentre glyphBoxes

    step :: Float -> GlyphInfo -> Picture
    step x gi
        | w <= 0 || h <= 0 = mempty
        | otherwise =
            Translate px py
                      (bitmapSection (Rectangle (gRect gi) (gSize gi))
                                     (atlasData c (pageOf gi)))
      where
        (w, h)  = gSize gi
        (px, py) = glyphCentre x gi

    -- FreeType placement: centre of the glyph bitmap.
    glyphCentre :: Float -> GlyphInfo -> (Float, Float)
    glyphCentre pen gi =
        ( pen + fromIntegral bx + fromIntegral w / 2
        , fromIntegral by - fromIntegral h / 2 )
      where
        (w, h)  = gSize gi
        (bx, by) = gBearing gi

    lookupGlyph c' = IM.findWithDefault (missingGlyph c') (fromEnum c') fontGlyphMap

-- | Pen positions for a sequence of glyphs: each pen is the x of the
--   glyph's origin; consecutive pens differ by advance + kerning pair.
penPositions :: [GlyphInfo] -> [Float]
penPositions = go 0
  where
    go _ [] = []
    go pen (gi : rest) =
        pen : go (pen + gAdvance gi + kern gi rest) rest
    -- kerning between this glyph and the next character
    kern :: GlyphInfo -> [GlyphInfo] -> Float
    kern gi (gi' : _) = kernPx gi gi'
    kern _ []         = 0

-- | Kerning adjustment between two adjacent glyphs (pixels).
kernPx :: GlyphInfo -> GlyphInfo -> Float
kernPx gi gi' =
    fromIntegral $ fromMaybe 0 $ do
        km <- IM.lookup (gChar gi) fontKerning
        IM.lookup (gChar gi') km

-- | Pixel size of the text picture at the raster size (bounding box).
--   Returns (0,0) for empty/invisible text.
textRasterSize :: Color -> String -> (Float, Float)
textRasterSize c str = (xmax - xmin, ymax - ymin)
  where
    gs   = map lookupGlyph str
    pens = penPositions gs
    boxes =
        [ let (px, py) = glyphCentre x gi
          in ( px - fromIntegral w / 2, py - fromIntegral h / 2
             , px + fromIntegral w / 2, py + fromIntegral h / 2 )
        | (x, gi) <- zip pens gs
        , let (w, h) = gSize gi
        , w > 0, h > 0 ]
    (xmin, ymin, xmax, ymax) =
        case foldr grow Nothing boxes of
          Nothing -> (0, 0, 0, 0)
          Just b  -> b
    glyphCentre :: Float -> GlyphInfo -> (Float, Float)
    glyphCentre pen gi =
        ( pen + fromIntegral bx + fromIntegral w / 2
        , fromIntegral by - fromIntegral h / 2 )
      where
        (w, h)  = gSize gi
        (bx, by) = gBearing gi
    grow :: (Float, Float, Float, Float) -> Maybe (Float, Float, Float, Float)
           -> Maybe (Float, Float, Float, Float)
    grow (xmin, ymin, xmax, ymax) Nothing =
        Just (xmin, ymin, xmax, ymax)
    grow (xmin, ymin, xmax, ymax) (Just (a, b, c, d)) =
        Just (min xmin a, min ymin b, max xmax c, max ymax d)
    lookupGlyph c' = IM.findWithDefault (missingGlyph c') (fromEnum c') fontGlyphMap

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

-- | Render UTF-8 text as a gloss-relative 'Frame' at the given size,
--   fitted according to 'TextFill' and aligned with 'Alignment'.
--
--   The text is scaled from the atlas raster size to 'TextSize', then
--   placed according to the fill mode:
--
--   * 'FillFit'      — 'Aspect' with the text's own aspect ratio and the
--                      requested alignment ('fit' in gloss-relative is
--                      @Aspect dim 'alignCenter' (Stretch (Just dim) pic)@;
--                      we generalise the alignment).
--   * 'FillStretch'  — 'stretch' the picture to fill the frame.
--   * 'FillNative'   — 'native' size placement.
utf8Banner :: TextSize -> TextFill -> Alignment -> String -> Color -> Frame
utf8Banner (TextSize sz) fill align txt c =
    case fill of
      FillFit     -> Aspect textDim align (Stretch (Just textDim) pic)
      FillStretch -> Stretch Nothing pic
      FillNative  -> native pic
  where
    pic   = Scale k k (utf8PictureColored c txt)
    k     = sz / rasterSize
    (tw, th) = textRasterSize c txt
    textDim = (tw * k, th * k)
    native :: Picture -> Frame
    native p = Sized $ \dim -> Stretch (Just dim) p

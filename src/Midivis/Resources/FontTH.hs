{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

-- | Compile-time font rasterizer (helper module).
--
--   Because GHC forbids top-level splices referencing locally-defined
--   functions (stage restriction), all the freetype2 rendering logic lives
--   here and is imported by "Midivis.Resources.Font", which performs the
--   splice.
module Midivis.Resources.FontTH
    ( GlyphInfo(..)
    , FontMetrics(..)
    , AtlasPage(..)
    , renderFontQ
    , renderFontCharsQ
    , renderAllGlyphs
    , renderCharsGlyphs
    , parseCharset
    , mergeFonts
    ) where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Unsafe         as BSU
import           Data.List                       ( maximumBy )
import           Data.Ord                        ( comparing )
import           Data.Word                       ( Word8 )
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Encoding.Error       as T
import           Control.Monad                   ( forM )
import           Data.Maybe                      ( catMaybes )
import           Foreign                         ( ForeignPtr, Ptr, Storable(..)
                                                , castPtr, mallocForeignPtrBytes
                                                , peek, pokeByteOff, plusPtr
                                                , withForeignPtr )
import           Foreign.C.Types
import           Language.Haskell.TH             ( Q, Exp, runIO )
import           Language.Haskell.TH.Syntax      ( Lift(..), qAddDependentFile )

import           FreeType.Core.Base
import           FreeType.Core.Types             ( FT_Bitmap(..), FT_Vector(..)
                                                , FT_ULong, FT_UInt )

-------------------------------------------------------------------------------
-- Public data types (re-exported by Midivis.Resources.Font)
-------------------------------------------------------------------------------

-- | Metadata for a single glyph (pixel coordinates in the atlas).
data GlyphInfo = GlyphInfo
    { gChar    :: !Int            -- ^ char code (fromEnum) of the glyph
    , gPage    :: !Int            -- ^ atlas page index
    , gRect    :: !(Int, Int)   -- ^ top-left corner (x,y) of the glyph in the page
    , gSize    :: !(Int, Int)   -- ^ width,height of the glyph bitmap
    , gBearing :: !(Int, Int)   -- ^ bitmap_left, bitmap_top (from the pen)
    , gAdvance :: !Float        -- ^ horizontal advance in pixels (not truncated)
    } deriving (Show, Eq)

instance Lift GlyphInfo where
    lift (GlyphInfo a b c d e f) =
        [| GlyphInfo a b c d e f |]

-- | Vertical metrics of the font at the rasterized pixel size.
data FontMetrics = FontMetrics
    { fmAscender  :: !Int
    , fmDescender :: !Int
    } deriving (Show, Eq)

instance Lift FontMetrics where
    lift (FontMetrics a b) = [| FontMetrics a b |]

-- | One RGBA atlas page (white glyphs, alpha = coverage).
data AtlasPage = AtlasPage
    { apSize  :: !(Int, Int)        -- ^ width,height of the page in pixels
    , apBytes :: !ByteString        -- ^ RGBA pixels, length = w*h*4
    } deriving (Show, Eq)

instance Lift AtlasPage where
    lift (AtlasPage a b) = [| AtlasPage a b |]

-------------------------------------------------------------------------------
-- Compile-time splice entry point
-------------------------------------------------------------------------------

-- | Rasterize every glyph of the font at compile time and embed the results.
--   The atlas byte strings are embedded via the 'Lift' instance of
--   'ByteString' (binary literal data), so no intermediate files are needed.
renderFontQ :: FilePath -> Int -> Q Exp
renderFontQ path px = do
    qAddDependentFile path
    bytes <- runIO $ BS.readFile path
    (glyphs, pages, fm, kern) <- runIO $ renderAllGlyphs bytes px
    [| (glyphs, pages, fm, kern) |]

-- | Rasterize only a specific set of characters (from a charset file)
--   at compile time.  Each non-comment, non-blank line of the charset file
--   holds exactly one character.
renderFontCharsQ :: FilePath -> FilePath -> Int -> Q Exp
renderFontCharsQ path charsetPath px = do
    qAddDependentFile path
    qAddDependentFile charsetPath
    bytes  <- runIO $ BS.readFile path
    csText <- runIO $ BS.readFile charsetPath
    (glyphs, pages, fm) <- runIO $ renderCharsGlyphs bytes (parseCharset csText) px
    -- supplementary chars (CJK/symbols) have no kerning table
    [| (glyphs, pages, fm, []) |]

-- | Parse a charset file: one character per line, '#' comments, blank lines
--   skipped.  A line may contain exactly one character.
parseCharset :: ByteString -> [Char]
parseCharset bs =
    [ c
    | ln <- lines (T.unpack (decodeUtf8Safe bs))
    , let c = firstNonSpaceNonHash ln
    , c /= '\0'
    ]
  where
    decodeUtf8Safe = T.decodeUtf8With T.lenientDecode
    firstNonSpaceNonHash :: String -> Char
    firstNonSpaceNonHash [] = '\0'
    firstNonSpaceNonHash (c : rest)
        | c == ' ' || c == '\t' || c == '\r' = firstNonSpaceNonHash rest
        | c == '#'                           = '\0'
        | otherwise                          = c

-- | Full pipeline rendering only the given characters (plus fallback: any
--   char requested that the font lacks yields an empty glyph record).
renderCharsGlyphs :: ByteString -> [Char] -> Int
                  -> IO ([(Char, GlyphInfo)], [AtlasPage], FontMetrics)
renderCharsGlyphs bytes chars px = ft_With_FreeType $ \lib ->
    BSU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
      ft_With_Memory_Face lib (castPtr cstr) (fromIntegral len) 0 $ \face -> do
        ft_Set_Pixel_Sizes face (fromIntegral px) (fromIntegral px)
        faceRec <- peek face
        sizeRec <- peek (frSize faceRec)
        let m = srMetrics sizeRec
            fm = FontMetrics { fmAscender  = fromIntegral (smAscender m) `div` 64
                             , fmDescender = fromIntegral (smDescender m) `div` 64 }
        raws <- collectChars face (frGlyph faceRec) chars
        packIntoAtlas fm raws

-- | Render the requested characters in order; characters not mapped by the
--   font are skipped (FT_Get_Char_Index == 0).
collectChars :: FT_Face -> FT_GlyphSlot -> [Char] -> IO [(FT_ULong, RawGlyph)]
collectChars face slotPtr = mapM renderOne . nubChars
  where
    renderOne :: Char -> IO (FT_ULong, RawGlyph)
    renderOne c = do
        let code = fromIntegral (fromEnum c) :: FT_ULong
        idx <- ft_Get_Char_Index face code
        if idx == 0
            then pure (code, emptyRaw)
            else do
                ft_Load_Char face code FT_LOAD_RENDER
                raw <- peekRawGlyph slotPtr
                pure (code, raw)
    nubChars = foldr (\c acc -> if c `elem` acc then acc else c : acc) []

emptyRaw :: RawGlyph
emptyRaw = RawGlyph 0 0 0 0 0 []

-- | Merge two font renders into one atlas: primary font glyphs first,
--   supplementary glyphs (only those not already in the primary) appended
--   on later pages.
--   Returns (merged glyphs, merged pages, primary metrics, primary kerning).
mergeFonts :: ([(Char, GlyphInfo)], [AtlasPage], FontMetrics, [(Int, Int, Int)])
           -> ([(Char, GlyphInfo)], [AtlasPage], FontMetrics, [(Int, Int, Int)])
           -> ([(Char, GlyphInfo)], [AtlasPage], FontMetrics, [(Int, Int, Int)])
mergeFonts (primGlyphs, primPages, primFm, primKern) (suppGlyphs, suppPages, _, _) =
    ( mergedGlyphs, primPages ++ suppPages, primFm, primKern )
  where
    primKeys = [ fromEnum c | (c, _) <- primGlyphs ]
    suppOnly = [ (c, gi) | (c, gi) <- suppGlyphs
                , fromEnum c `notElem` primKeys ]
    -- shift supplementary page indices by the number of primary pages
    shift :: Int -> GlyphInfo -> GlyphInfo
    shift n gi = gi { gPage = gPage gi + n }
    nPrim = length primPages
    mergedGlyphs = primGlyphs
        ++ [ (c, shift nPrim gi) | (c, gi) <- suppOnly ]

-- | Full compile-time pipeline.
--   Returns (glyph metadata list, atlas pages, font metrics, kerning table).
--   The kerning table is @[(leftChar, rightChar, kernPx)]@ for the Latin
--   charset (the supplementary CJK/symbol glyphs have no kerning).
renderAllGlyphs :: ByteString -> Int
                -> IO ([(Char, GlyphInfo)], [AtlasPage], FontMetrics, [(Int, Int, Int)])
renderAllGlyphs bytes px = ft_With_FreeType $ \lib ->
    BSU.unsafeUseAsCStringLen bytes $ \(cstr, len) ->
      ft_With_Memory_Face lib (castPtr cstr) (fromIntegral len) 0 $ \face -> do
        ft_Set_Pixel_Sizes face (fromIntegral px) (fromIntegral px)
        faceRec <- peek face

        -- vertical metrics from size metrics (26.6 fixed-point, so ÷64)
        sizeRec <- peek (frSize faceRec)
        let m = srMetrics sizeRec
            fm = FontMetrics { fmAscender  = fromIntegral (smAscender m) `div` 64
                             , fmDescender = fromIntegral (smDescender m) `div` 64 }

        -- iterate over every char in the font, collecting raw glyphs
        raws <- collectRaws face (frGlyph faceRec)
        -- pack the raw glyphs into atlas pages
        (glyphs, pages, _) <- packIntoAtlas fm raws
        -- kerning for the Latin charset
        kern <- collectKerning face latinCharset
        pure (glyphs, pages, fm, kern)

-------------------------------------------------------------------------------
-- Glyph rasterization
-------------------------------------------------------------------------------

-- | Raw glyph data read from the glyph slot (before packing into an atlas).
data RawGlyph = RawGlyph
    { rgW       :: !Int
    , rgH       :: !Int
    , rgLeft    :: !Int
    , rgTop     :: !Int
    , rgAdvance :: !Float        -- ^ horizontal advance in pixels (26.6 ÷64)
    , rgRows    :: [ByteString]  -- each row of coverage bytes (top to bottom)
    }

collectRaws :: FT_Face -> FT_GlyphSlot -> IO [(FT_ULong, RawGlyph)]
collectRaws face slotPtr = go Nothing
  where
    go :: Maybe FT_ULong -> IO [(FT_ULong, RawGlyph)]
    go mcode = do
        (code, _) <- ft_Get_Next_Char face mcode
        if code == 0
            then pure []
            else do
                ft_Load_Char face code FT_LOAD_RENDER
                raw <- peekRawGlyph slotPtr
                rest <- go (Just code)
                pure ((code, raw) : rest)

-- | Read the currently-loaded glyph out of the glyph slot.
--   The slot buffer is owned by FreeType and reused on the next load,
--   so we copy every row immediately.
peekRawGlyph :: FT_GlyphSlot -> IO RawGlyph
peekRawGlyph slotPtr = do
    slot <- peek slotPtr
    let bm    = gsrBitmap slot
        w     = fromIntegral (bWidth bm)  :: Int
        h     = fromIntegral (bRows bm)   :: Int
        pitch = fromIntegral (bPitch bm)  :: Int
        buf   = bBuffer bm
        advX  = fromIntegral (vX $ gsrAdvance slot) :: Float
        left  = fromIntegral (gsrBitmap_left slot)  :: Int
        top   = fromIntegral (gsrBitmap_top slot)   :: Int
    rows <- if w <= 0 || h <= 0
        then pure []
        else mapM (copyRow buf pitch w) [0 .. h - 1]
    pure $ RawGlyph w h left top (advX / 64) rows
  where
    copyRow :: Ptr Word8 -> Int -> Int -> Int -> IO ByteString
    copyRow buf pitch w i =
        BS.packCStringLen (castPtr (buf `plusPtr` (i * pitch)), w)

-------------------------------------------------------------------------------
-- Kerning
-------------------------------------------------------------------------------

-- | Compute a kerning table for all pairs of the given characters.
--   Returns a sparse list: @[(leftChar, rightChar, kernPx)]@ with @kernPx@
--   non-zero.  Only characters actually mapped by the font contribute.
collectKerning :: FT_Face -> [Char] -> IO [(Int, Int, Int)]
collectKerning face chars = do
    -- map each char to its glyph index; skip unmapped
    pairs <- forM chars $ \c -> do
        let code = fromIntegral (fromEnum c) :: FT_ULong
        idx <- ft_Get_Char_Index face code
        pure (fromEnum c, idx)
    let mapped :: [(Int, FT_UInt)]
        mapped = [ (c, idx) | (c, idx) <- pairs, idx /= 0 ]
    -- nested: forM over left chars, each yielding a list of (right,kern)
    fmap (concatMap catMaybes) $ forM mapped $ \(lc, li) ->
        forM mapped $ \(rc, ri) -> do
            kern <- ft_Get_Kerning face li ri FT_KERNING_DEFAULT
            let k = round (fromIntegral (vX kern) / 64 :: Float)
            pure $ if k == 0 then Nothing else Just (lc, rc, k)

-- | Kerning pairs needed for common Latin text (per the font's kern table).
--   The primary font (GoogleSans) covers Latin; supplementary CJK glyphs
--   have no kerning, so we only compute it for the primary render.
latinCharset :: [Char]
latinCharset = [' ' .. '~']  -- ASCII 32..126

-------------------------------------------------------------------------------
-- Atlas packing
-------------------------------------------------------------------------------

pageWidth, pageHeight :: Int
pageWidth  = 2048
pageHeight = 2048

-- | Place glyphs into shelves.  Pure layout: returns for each glyph the
--   page index and its top-left corner (x,y), glyphs sorted by height desc.
layoutShelves :: [(FT_ULong, RawGlyph)] -> [(FT_ULong, RawGlyph, Int, Int, Int)]
layoutShelves = go 0 0 0 0
  where
    go :: Int -> Int -> Int -> Int
       -> [(FT_ULong, RawGlyph)]
       -> [(FT_ULong, RawGlyph, Int, Int, Int)]
    go _ _ _ _ [] = []
    go pg x y rowH ((c, g) : rest)
        | gw > pageWidth || gh > pageHeight =
            -- too big for any page: place at origin of a fresh page anyway
            (c, g, 0, 0, pg) : go (pg + 1) 0 0 0 rest
        | x + gw > pageWidth =
            -- wrap to a new shelf row (same page)
            go pg 0 (y + rowH) 0 ((c, g) : rest)
        | y + gh > pageHeight =
            -- start a new page
            (c, g, 0, 0, pg + 1) : go (pg + 1) gw gh gh rest
        | otherwise =
            (c, g, x, y, pg) : go pg (x + gw) y (max rowH gh) rest
      where
        -- +1px padding on every side of the glyph
        gw = rgW g + 2
        gh = rgH g + 2

-- | Build RGBA pages from the layout, blitting glyphs into writable buffers.
buildPages :: [(FT_ULong, RawGlyph, Int, Int, Int)]
           -> IO [(Int, Int, ForeignPtr Word8)]
buildPages placed = do
    let byPage = groupByPage placed
    mapM buildPage byPage
  where
    buildPage :: [(FT_ULong, RawGlyph, Int, Int, Int)]
              -> IO (Int, Int, ForeignPtr Word8)
    buildPage entries = do
        fptr <- mallocForeignPtrBytes (pageWidth * pageHeight * 4)
        withForeignPtr fptr $ \base -> do
            fillZero base (pageWidth * pageHeight * 4)
            mapM_ (blit base) entries
        let usedH = maximumBy (comparing snd)
                        [ (c, y + rgH g) | (c, g, _, y, _) <- entries ]
        pure (pageWidth, max 1 (snd usedH), fptr)
      where
        blit :: Ptr Word8 -> (FT_ULong, RawGlyph, Int, Int, Int) -> IO ()
        blit base (_, g, x, y, _) =
            forEachRow (rgRows g) $ \i row ->
                forEachCol (BS.unpack row) $ \j cov -> do
                    let dst = base `plusPtr` (((y + 1 + i) * pageWidth + (x + 1 + j)) * 4)
                    pokeByteOff dst 0 (255 :: Word8)
                    pokeByteOff dst 1 (255 :: Word8)
                    pokeByteOff dst 2 (255 :: Word8)
                    pokeByteOff dst 3 (cov :: Word8)

    groupByPage :: [(FT_ULong, RawGlyph, Int, Int, Int)]
                -> [[(FT_ULong, RawGlyph, Int, Int, Int)]]
    groupByPage = foldr insert []
      where
        insert e [] = [[e]]
        insert e (p : ps)
            | ePage e == ePage (head p) = (e : p) : ps
            | otherwise                 = [e] : p : ps
        ePage (_, _, _, _, pg) = pg

    fillZero :: Ptr Word8 -> Int -> IO ()
    fillZero p n = go p n
      where
        go _ 0 = pure ()
        go p' i = pokeByteOff p' 0 (0 :: Word8) >> go (p' `plusPtr` 1) (i - 1)

    forEachRow :: [a] -> (Int -> a -> IO ()) -> IO ()
    forEachRow xs f = go 0 xs
      where
        go _ [] = pure ()
        go i (x : rest) = f i x >> go (i + 1) rest

    forEachCol :: [a] -> (Int -> a -> IO ()) -> IO ()
    forEachCol xs f = go 0 xs
      where
        go _ [] = pure ()
        go j (x : rest) = f j x >> go (j + 1) rest

-- | Freeze built pages into AtlasPage values.
freezePages :: [(Int, Int, ForeignPtr Word8)] -> IO [AtlasPage]
freezePages = mapM freeze
  where
    freeze (w, h, fptr) = do
        bs <- withForeignPtr fptr $ \p ->
            BS.packCStringLen (castPtr p, w * h * 4)
        pure $ AtlasPage (w, h) bs

-- | Turn raw glyphs into final metadata + atlas pages.
packIntoAtlas :: FontMetrics -> [(FT_ULong, RawGlyph)]
              -> IO ([(Char, GlyphInfo)], [AtlasPage], FontMetrics)
packIntoAtlas fm raws = do
    let sorted = sortBy (flip (comparing (rgH . snd))) raws
        placed = layoutShelves sorted
        -- glyph infos from the layout.  Keep glyphs that have pixels OR a
        -- non-zero advance (e.g. space characters: empty bitmap, but they
        -- must carry their advance width into the glyph map).
        infos = [ (toEnum (fromIntegral c), mkInfo (c, g, x, y, pg))
                | (c, g, x, y, pg) <- placed
                , rgW g > 0 || rgAdvance g > 0 ]
    pages <- buildPages placed >>= freezePages
    pure (infos, pages, fm)
  where
    mkInfo (c, g, x, y, pg) =
        GlyphInfo { gChar    = fromIntegral c
                  , gPage    = pg
                  , gRect    = (x + 1, y + 1)
                  , gSize    = (rgW g, rgH g)
                  , gBearing = (rgLeft g, rgTop g)
                  , gAdvance = rgAdvance g
                  }

-- simple insertion sort descending by key (small n)
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr insert []
  where
    insert x [] = [x]
    insert x ys@(y : rest) =
        case cmp x y of
          GT -> x : ys
          _  -> y : insert x rest

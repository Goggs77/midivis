{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time font rendering cache (public interface).
--
--   Delegates the heavy lifting to "Midivis.Resources.FontTH" (which must be
--   a separate module because GHC's stage restriction forbids top-level
--   splices referencing locally-defined functions).  This module performs the
--   splices and exposes the merged glyph cache:
--
--   * @fontGlyphMap@ — glyph metadata for the primary font (GoogleSans,
--     every glyph of the font) plus supplementary glyphs (NotoSerifSC,
--     only the characters listed in @assets/fonts/charset.txt@),
--   * @fontMetrics@  — ascender / descender at the raster size,
--   * @fontAtlas@    — RGBA atlas pages (white glyphs + coverage alpha),
--   * @fontKerning@  — Latin kerning pairs (leftChar, rightChar, kernPx).
module Midivis.Resources.Font
    ( GlyphInfo(..)
    , FontMetrics(..)
    , AtlasPage(..)
    , fontGlyphMap
    , fontMetrics
    , fontAtlas
    , fontKerning
    , missingGlyph
    , renderAllGlyphs
    , renderCharsGlyphs
    , parseCharset
    ) where

import qualified Data.IntMap.Strict             as IM
import           Data.IntMap.Strict             ( IntMap )

import           Midivis.Resources.FontTH

-- | Compile-time splices.
--   Primary: every glyph of GoogleSans at 64px.
--   Supplementary: only the chars listed in assets/fonts/charset.txt,
--   rendered from NotoSerifSC at 64px.
--   Each result is (glyphs, pages, metrics, kerning).
fontData :: ([(Char, GlyphInfo)], [AtlasPage], FontMetrics, [(Int, Int, Int)])
fontData = mergeFonts
    $(renderFontQ "assets/fonts/GoogleSans-Regular.ttf" 64)
    $(renderFontCharsQ "assets/fonts/NotoSerifSC-Regular.ttf"
                       "assets/fonts/charset.txt" 64)

-- | Glyph lookup table, keyed by 'fromEnum' of the character.
fontGlyphMap :: IntMap GlyphInfo
fontGlyphMap = IM.fromList [ (fromEnum c, gi) | (c, gi) <- fst4 fontData ]

-- | Font metrics (ascender / descender in pixels at 64px).
fontMetrics :: FontMetrics
fontMetrics = thd4 fontData

-- | Atlas pages (RGBA, white + coverage alpha).
fontAtlas :: [AtlasPage]
fontAtlas = snd4 fontData

-- | Kerning table: (leftChar, rightChar) -> horizontal offset in pixels.
--   Sparse; only non-zero pairs of the Latin charset are stored.
fontKerning :: IntMap (IntMap Int)
fontKerning =
    IM.fromListWith IM.union
        [ (lc, IM.singleton rc k)
        | (lc, rc, k) <- fth4 fontData ]

-- | Glyph used for characters not present in the font (invisible).
missingGlyph :: Char -> GlyphInfo
missingGlyph c =
    GlyphInfo { gChar    = fromEnum c
              , gPage    = 0
              , gRect    = (0, 0)
              , gSize    = (0, 0)
              , gBearing = (0, 0)
              , gAdvance = 0
              }

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a
snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b
thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c
fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

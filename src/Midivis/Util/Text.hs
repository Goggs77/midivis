{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Midivis.Util.Text where
import Data.Text as T
import Data.FileEmbed
import Data.Text.Encoding (decodeUtf8)
import Data.Char (isUpper, isLower)

blanks :: Text
blanks = decodeUtf8 $(embedFileRelative "assets/text/blanks.txt")

takeShow :: (Show a, Integral b) => b -> a -> T.Text
takeShow b a = let t = T.show a in if (fromIntegral $ T.length t) > b then T.take (fromIntegral b) t else t

--floatShow‰ :: (RealFrac a, Show a) => a -> Text
floatShow :: (RealFrac a) => a -> Text
floatShow f = T.concat [T.show (round (f * 1000.0)), "  ‰"]

isBlank :: T.Text -> Bool
isBlank t= T.null $ T.dropWhile (`T.elem` blanks) t

firstLow :: T.Text -> T.Text
firstLow ts = T.concat [T.toLower $ T.takeWhile isUpper ts, T.dropWhile isLower ts]

toSubscript :: Char -> Char
toSubscript a = case a of
    '0' -> '₀'
    '1' -> '₁'
    '2' -> '₂'
    '3' -> '₃'
    '4' -> '₄'
    '5' -> '₅'
    '6' -> '₆'
    '7' -> '₇'
    '8' -> '₈'
    '9' -> '₉'
    '+' -> '₊'
    '-' -> '₋'
    'e' -> 'ₑ'
    '.' -> '.'
    other -> other

subscriptNum :: (Num a, Show a) => a -> T.Text
subscriptNum num = T.map toSubscript (T.show num)
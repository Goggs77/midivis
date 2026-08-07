{-# LANGUAGE TemplateHaskell #-}
module Midivis.Util.Text where
import Data.Text as T
import Data.FileEmbed
import Data.Text.Encoding (decodeUtf8)
import Data.Char (isUpper, isLower)

blanks :: Text
blanks = decodeUtf8 $(embedFileRelative "assets/text/blanks.txt")

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
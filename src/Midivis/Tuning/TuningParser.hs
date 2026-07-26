{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Midivis.Tuning.TuningParser(Tuning(..), Scala(..), parseScala, makeScala) where
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.List.Extra ( trim )
import Data.Ratio ( (%) )

import Midivis.Util.THUtils
import Language.Haskell.TH
import Data.Char (toLower)
    
--MIDI supports [0 127] note range. we fit each note periodically
class Tuning a where
    period :: a -> Integer
    lowestFreq :: a -> Double --unset in .scl file
    calibrate :: Integral b => a -> b -> Double -> a
    calibrateA4 :: a -> Double -> a
    calibrateC4 :: a -> Double -> a
    getFreq :: Integral b => a -> b -> Double
    getGeneralName :: Integral b => a -> b -> String



-- .scl files forces ASCII encoding but UTF-8 has this down-ward compatibility
safeConvert :: BS.ByteString -> String
safeConvert bs =
    let
        eth = decodeUtf8' bs
        dec (Left _) = error "Invalid UTF-8 encoding"
        dec (Right s) = T.unpack s
    in
        dec eth

isBlank :: String -> Bool
isBlank str = null (dropWhile isSpace str)
    where isSpace c = or [c==' ', c=='\t', c=='\n'] -- add more utf-8 spaces? no

removeCommentsPreP :: BS.ByteString -> [String]
removeCommentsPreP scl =
    let
        rcls = map (takeWhile (\c -> c /='!')) -- remove trailing comments
            $ filter (\s -> s !! 0 /= '!' && (not $ isBlank s)) -- remove comment lines  
            $ (lines (safeConvert scl)) -- get string
    in
        map trim rcls
        --after removing all comments we should get this format:
        -- 1: <synopsis>
        -- 2: <n for 'note counts per period'>
        -- 3 to 2+n : <pitch values>    
        -- "The first note of 1/1 or 0.0 cents is implicit and not in the files."
        -- The last line usually == 2/1 (or 1200.0), which means base tone +1 octave
        --about the pitches:
        -- any decimal number will be treated as number with a unit(cent)
        --  100 cent <=> *2^(1/12) (in frequency)
        -- any integer will be seen as ratios
        --more on https://www.huygens-fokker.org/scala/scl_format.html, they even support parsing unit explicitly

data Scala = Scala
    {
        name :: String, -- unset above
        synopsis :: String,
        count :: Integer,
        pitches :: [Double]
    }

countChar :: Eq a => a -> [a] -> Int
countChar x xs = length (filter (== x) xs)

parsePitch :: String -> Double -- times
parsePitch s =
    let
        cdot = countChar '.' s
        cslash = countChar '/' s
        s' = takeWhile (isNumChar) (trim s)
            where isNumChar c = or [c `elem` ['0'..'9'], c == '/', c == '.']
    in 
        if or [cdot >= 2, cslash >=2, cdot * cslash /= 0] 
            then error "Invalid pitch format in .scl" 
            else case (cdot, cslash) of
                (0, 0) -> read s' :: Double
                (0, 1) -> let 
                    p = read (takeWhile (/='/') s') :: Double
                    q = read (drop 1 $ dropWhile (/='/') s') :: Double
                    in p/q
                (1, 0) -> let
                    cents = read s' :: Double
                    in 2.0**(cents/1200.0)
                _ -> error "Invalid pitch format in .scl"
                        

parseScala :: BS.ByteString -> Scala
parseScala str =
    let 
        perp = removeCommentsPreP str -- cooked lines
        snps = perp !! 0 -- synopsis
        cnt = read (perp !! 1) :: Integer
        ps = drop 2 perp
        valid = fromIntegral cnt == length ps
    in
        if valid 
            then Scala "" snps cnt (map parsePitch ps) 
            else error "Mismatch between declared note count and the actual line count in .scl"


makeScala :: String -> Scala -> Q [Dec]
makeScala name' (Scala _ syn cnt pit) = do
    let firstLow (c:cs) = (toLower c) : cs
        count = mkName "count"
        name = mkName "scl_name"
        newScl = mkName (firstLow name')
        pitches = mkName "pitches"
        scl = mkName "scl"
        scl_count = mkName "scl_count"
        scl_pitches = mkName "scl_pitches"
        scl_synopsis = mkName "scl_synopsis"
        qName = qkExpToValD name (LitE (StringL name'))
        qSyn = qkExpToValD scl_synopsis (LitE (StringL syn))
        qCount = qkExpToValD scl_count (LitE (IntegerL cnt))
        qPitches = qkListDouble scl_pitches (pit)
        qScl =
            ValD
            (VarP newScl)
            ( NormalB
                ( AppE
                    ( AppE
                        (AppE (AppE (ConE 'Scala) (VarE name)) (VarE scl_synopsis))
                        (VarE scl_count)
                    )
                    (VarE scl_pitches)
                )
            )
            []
    return [qName, qSyn, qCount, qPitches, qScl]


--deprecated
makeTuning :: String -> Scala -> Q [Dec]
makeTuning sclName (Scala _ syn cnt pit) = do
    let newdata = mkName sclName
        calibrate = mkName "calibrate"
        calibrateA4 = mkName "calibrateA4"
        calibrateC4 = mkName "calibrateC4"
        --count = mkName "count"
        f = mkName "f"
        freq = mkName "freq"
        fromIntegral = mkName "fromIntegral"
        getFreq = mkName "getFreq"
        getGeneralName = mkName "getGeneralName"
        lowest = mkName "lowest"
        lowestFreq = mkName "lowestFreq"
        n = mkName "n"
        newFreq = mkName "newFreq"
        newlow = mkName "newlow"
        noteID = mkName "noteID"
        period = mkName "period"
        --pitches = mkName "pitches"
        prod = mkName "prod"
        q = mkName "q"
        quotRem = mkName "quotRem"
        r = mkName "r"
        scl_count = mkName "scl_count"
        scl_pitches = mkName "scl_pitches"
        show = mkName "show"
        synopsis = mkName "synopsis"
        --sorry about this
        qSyn = qkExpToValD synopsis (LitE (StringL syn))
        qCount = qkExpToValD scl_count (LitE (IntegerL cnt))
        qPitches = qkListDouble scl_pitches (pit)
        qData = DataD [] newdata [] Nothing [RecC newdata [(lowest,Bang NoSourceUnpackedness NoSourceStrictness,ConT ''Double)]] []
        qInstance = InstanceD Nothing [] (AppT (ConT ''Tuning) (ConT newdata)) [FunD period [Clause [WildP] (NormalB (VarE scl_count)) []],FunD lowestFreq [Clause [ParensP (ConP newdata [] [VarP lowest])] (NormalB (VarE lowest)) []],FunD calibrate [Clause [ParensP (ConP newdata [] [WildP]),VarP noteID,VarP newFreq] (NormalB (AppE (ConE newdata) (VarE newlow))) [ValD (TupP [VarP q,VarP r]) (NormalB (UInfixE (AppE (VarE fromIntegral) (VarE noteID)) (VarE quotRem) (VarE scl_count))) [],ValD (VarP prod) (NormalB (CondE (UInfixE (VarE r) (VarE (mkName "/=")) (LitE (IntegerL 0))) (UInfixE (VarE scl_pitches) (VarE (mkName "!!")) (ParensE (UInfixE (AppE (VarE fromIntegral) (VarE r)) (VarE (mkName "-")) (LitE (IntegerL 1))))) (LitE (IntegerL 1)))) [],ValD (VarP newlow) (NormalB (UInfixE (VarE newFreq) (VarE (mkName "/")) (ParensE (UInfixE (VarE prod) (VarE (mkName "*")) (UInfixE (LitE (RationalL (2 % 1))) (VarE (mkName "**")) (AppE (VarE fromIntegral) (VarE q))))))) []]],FunD calibrateA4 [Clause [VarP n,VarP f] (NormalB (AppE (AppE (AppE (VarE calibrate) (VarE n)) (LitE (IntegerL 69))) (VarE f))) []],FunD calibrateC4 [Clause [VarP n,VarP f] (NormalB (AppE (AppE (AppE (VarE calibrate) (VarE n)) (LitE (IntegerL 60))) (VarE f))) []],FunD getFreq [Clause [ParensP (ConP newdata [] [VarP lowest]),VarP noteID] (NormalB (VarE freq)) [ValD (TupP [VarP q,VarP r]) (NormalB (UInfixE (AppE (VarE fromIntegral) (VarE noteID)) (VarE quotRem) (VarE scl_count))) [],ValD (VarP prod) (NormalB (CondE (UInfixE (VarE r) (VarE (mkName "/=")) (LitE (IntegerL 0))) (UInfixE (VarE scl_pitches) (VarE (mkName "!!")) (ParensE (UInfixE (AppE (VarE fromIntegral) (VarE r)) (VarE (mkName "-")) (LitE (IntegerL 1))))) (LitE (IntegerL 1)))) [],ValD (VarP freq) (NormalB (UInfixE (VarE lowest) (VarE (mkName "*")) (ParensE (UInfixE (VarE prod) (VarE (mkName "*")) (UInfixE (LitE (RationalL (2 % 1))) (VarE (mkName "**")) (AppE (VarE fromIntegral) (VarE q))))))) []]],FunD getGeneralName [Clause [WildP,VarP noteID] (NormalB (LetE [ValD (TupP [VarP q,VarP r]) (NormalB (UInfixE (AppE (VarE fromIntegral) (VarE noteID)) (VarE quotRem) (AppE (VarE fromIntegral) (VarE scl_count)))) []] (UInfixE (AppE (VarE show) (VarE r)) (VarE (mkName "++")) (UInfixE (LitE (StringL "/")) (VarE (mkName "++")) (AppE (VarE show) (VarE q)))))) []]]
    return [qSyn, qCount, qPitches, qData, qInstance]



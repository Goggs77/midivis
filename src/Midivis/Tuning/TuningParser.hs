{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Midivis.Tuning.TuningParser
    ( Scala(..), parseScala, makeScala
    , Ascl, parseAscl, makeAscl
    , parseAsclDirective, parseAsclDirectives
    ) where
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.List.Extra ( trim )
import Data.Maybe (mapMaybe)

import Midivis.Util.THUtils
import qualified Midivis.Util.Text as T
import Midivis.Tuning.Scala
import qualified Midivis.Tuning.Ascl as A hiding (c0)
import Language.Haskell.TH
import Data.Char (toLower, isUpperCase)
import Midivis.Tuning.Ascl (AsclDirective(..), AsclDirectiveSet (referencePitch, noteNames, noteRange, tuningSource, sourceLink), Ascl (..), defReferencePitch, defDirectiveSet, defNoteNames, defNoteRange, defSource, defLink)
    
--MIDI supports [0 127] note range. we fit each note periodically




-- | .scl files forces ASCII encoding but UTF-8 has this downward compatibility.
--  Actually unsafe in terms of error processing but this is compile-time anyway.
unsafeConvert :: BS.ByteString -> String
unsafeConvert  = T.unpack . decodeUtf8 
   

unsafeConvert' :: BS.ByteString -> T.Text
unsafeConvert' = decodeUtf8

isBlank :: String -> Bool
isBlank str = null (dropWhile isSpace str)
    where isSpace c = or [c==' ', c=='\t', c=='\n'] -- add more utf-8 spaces? no

removeCommentsScala :: BS.ByteString -> [String]
removeCommentsScala scl =
    map (takeWhile (\c -> c /='!')) -- remove trailing comments
    $ filter (\s -> s !! 0 /= '!' && (not $ isBlank s)) -- remove comment lines  
    $ (map trim $ lines (unsafeConvert scl)) -- get string

-- | Extract the "@ABL ..." directive lines from an ASCL file.
--   ABL extensions live inside SCL comment lines ("! @ABL ..."); per the spec
--   a second leading "!" ("!! @ABL ...") disables that directive and it is
--   ignored.  Returns the text after the leading "!"s ("@ABL ...").
removeCommentsAscl :: BS.ByteString -> [T.Text]
removeCommentsAscl scl =
    mapMaybe extractABL $ T.lines (unsafeConvert' scl)
  where
    extractABL :: T.Text -> Maybe T.Text
    extractABL raw =
        let s = T.strip raw
        in if T.isPrefixOf "!!" s
               then Nothing                              -- disabled directive
               else if T.isPrefixOf "! @" s
                    then Just $ T.strip (T.dropWhile (== '!') s)
                    else Nothing

-- | Split on whitespace honouring double-quoted arguments ("E koron" stays a
--   single token with its inner space preserved; only the quotes are
--   stripped).  An unbalanced quote swallows the rest of the line — malformed
--   files surface as wrong token counts later.
splitQuoted :: T.Text -> [T.Text]
splitQuoted t = reverse (go False (T.unpack t) [] [])
  where
    -- go inQuotes chars tokens cur: tokens reversed; cur is the current
    -- token's characters reversed (cons is O(1), we reverse once on flush)
    go :: Bool -> String -> [T.Text] -> String -> [T.Text]
    go _ [] acc cur = if null cur then acc else T.pack (reverse cur) : acc
    go inQ (c:cs) acc cur = case c of
        '"' -> go (not inQ) cs acc cur
        _ | c == ' ' || c == '\t' ->
                if null cur || inQ
                    -- inside quotes (or between tokens): keep the space in
                    -- the current token so "E koron" does not split
                    then go inQ cs acc (if null cur then cur else c : cur)
                    else go inQ cs (T.pack (reverse cur) : acc) ""
          | otherwise -> go inQ cs acc (c : cur)
    
        --after removing all comments we should get this format:
        -- 1: <synopsis>
        -- 2: <n for 'note counts per period'>
        -- 3 to 2+n : <pitch values> (and for .ascl we have) ! <name for each note>
        -- "The first note of 1/1 or 0.0 cents is implicit and not in the files."
        -- The last line usually == 2/1 (or 1200.0), which means base tone +1 octave
        --about the pitches:
        -- any decimal number will be treated as number with a unit(cent)
        --  100 cent <=> *2^(1/12) (in frequency)
        -- any integer will be seen as ratios
        --more on https://www.huygens-fokker.org/scala/scl_format.html, they even support parsing unit explicitly


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
            then error "Invalid pitch format in .scl/.ascl file" 
            else case (cdot, cslash) of
                (0, 0) -> read s' :: Double
                (0, 1) -> let 
                    p = read (takeWhile (/='/') s') :: Double
                    q = read (drop 1 $ dropWhile (/='/') s') :: Double
                    in p/q
                (1, 0) -> let
                    cents = read (s'++"0") :: Double
                    in 2.0**(cents/1200.0)
                _ -> error "Invalid pitch format in .scl/.ascl file"

parsePitchAscl :: T.Text -> (Double, T.Text)
parsePitchAscl sn =
    let
        s = T.strip $ T.takeWhile (/='!') sn
        -- comment name after '!': keep inner spaces, strip surrounding quotes
        n = T.dropAround (== '"') $ T.strip $ T.dropWhile (/='!') sn
    in 
        (parsePitch $ T.unpack s, n)


parseScala :: BS.ByteString -> Scala
parseScala str =
    let 
        l = removeCommentsScala str -- cooked lines
        snps = l !! 0 -- synopsis
        cnt = read (l !! 1) :: Integer
        ps = drop 2 l
        valid = fromIntegral cnt == length ps
    in
        if valid 
            then Scala "" c0 snps cnt (map parsePitch ps) 
            else error "Mismatch between declared note count and the actual line count in .scl"


makeScala :: String -> Scala -> Q [Dec]
makeScala name' (Scala _ low syn cnt pit) = do
    let firstLow cs = map toLower (takeWhile isUpperCase cs) ++ (dropWhile isUpperCase cs)
        count = mkName "count"
        name = mkName "scl_name"
        newScl = mkName (firstLow name')
        pitches = mkName "pitches"
        scl = mkName "scl"
        scl_lowest = mkName "scl_lowest"
        scl_count = mkName "scl_count"
        scl_pitches = mkName "scl_pitches"
        scl_synopsis = mkName "scl_synopsis"
        qName = qkExpToValD name (LitE (StringL name'))
        qLowest = qkExpToValD scl_lowest (LitE (RationalL (toRational low)))
        qSyn = qkExpToValD scl_synopsis (LitE (StringL syn))
        qCount = qkExpToValD scl_count (LitE (IntegerL cnt))
        qPitches = qkListDouble scl_pitches (pit)
        qScl =
            ValD
              (VarP newScl)
              ( NormalB
                  ( AppE
                      ( AppE
                          ( AppE
                              (AppE (AppE (ConE 'Scala) (VarE name)) (VarE scl_lowest))
                              (VarE scl_synopsis)
                          )
                          (VarE scl_count)
                      )
                      (VarE scl_pitches)
                  )
              )
              --where
              [qName, qLowest, qSyn, qCount, qPitches]
    return [qScl]

-- | Feed me "@ABL <Directive Name> <*args>"
parseAsclDirective :: T.Text -> A.AsclDirective
parseAsclDirective text = 
    let
        w = T.words text
        w' = map T.unpack w
        valid = w !! 0 == "@ABL"
    in  if not valid then error $ "Unknown identifier " ++ w' !! 0 else 
        case w !! 1 of
        "REFERENCE_PITCH" -> REFERENCE_PITCH 
            (read (w' !! 2) :: Integer)
            (read (w' !! 3) :: Integer)
            (read (w' !! 4) :: Double)
        -- NOTE_NAMES may carry quoted names containing spaces ("E koron")
        "NOTE_NAMES" -> NOTE_NAMES $ splitQuoted (T.strip (T.unwords (drop 2 w)))
        "NOTE_RANGE_BY_FREQUENCY" -> NOTE_RANGE_BY_FREQUENCY
            (read (w' !! 2) :: Double)
            (read (w' !! 3) :: Double)
        -- max octave/index are optional per the spec; when absent we widen
        -- the range generously (min + 10 octaves) so the note stays usable.
        "NOTE_RANGE_BY_INDEX" -> case length w' of
            4 -> let o = read (w' !! 2) :: Integer
                     i = read (w' !! 3) :: Integer
                 in NOTE_RANGE_BY_INDEX o i (o + 10) i
            6 -> NOTE_RANGE_BY_INDEX
                    (read (w' !! 2) :: Integer)
                    (read (w' !! 3) :: Integer)
                    (read (w' !! 4) :: Integer)
                    (read (w' !! 5) :: Integer)
            _ -> error $ "NOTE_RANGE_BY_INDEX expects 2 or 4 arguments, got " ++ show (length w' - 2)
        "SOURCE" -> SOURCE $ T.unwords (drop 2 w)
        "LINK" -> LINK $ w !! 2
        _ -> error $ "Unknown @ABL directive \"" ++ w' !! 1 ++ "\"."

-- | Feed me lines of "@ABL <Directive Name> <*args>"
parseAsclDirectives :: [T.Text] -> AsclDirectiveSet
parseAsclDirectives lines = 
    let
        directives = map parseAsclDirective lines
        extractDirective :: AsclDirective -> (AsclDirective, AsclDirective, AsclDirective, AsclDirective, AsclDirective)
                         -> (AsclDirective, AsclDirective, AsclDirective, AsclDirective, AsclDirective)
        extractDirective d (r, n, nr, s, l) = case d of
            REFERENCE_PITCH {} -> (d, n, nr, s, l)
            NOTE_NAMES {} -> (r, d, nr, s, l)
            NOTE_RANGE_BY_FREQUENCY {} -> (r, n, d, s, l)
            NOTE_RANGE_BY_INDEX {} -> (r, n, d, s, l)
            SOURCE {} -> (r, n, nr, d, l)
            LINK {} -> (r, n, nr, s, d)
        (refPitch, noteNms, noteRng, src, lnk) = 
            foldr extractDirective 
                  (defReferencePitch, defNoteNames, defNoteRange, defSource, defLink)
                  directives
    in
        defDirectiveSet 
            { 
              referencePitch = refPitch
            , noteNames = noteNms
            , noteRange = noteRng
            , tuningSource = src
            , sourceLink = lnk 
            }


-- | Parse an ASCL file (a UTF-8 SCL superset).
--   Structure: <synopsis> / <count> / <count pitch lines, each "pitch [! name]",
--   the last pitch being the pseudo-octave (usually 2/1 or 1200.0)> /
--   "@ABL ..." directive lines inside comments.
parseAscl :: BS.ByteString -> Ascl
parseAscl str =
    let rawLines = map T.strip $ T.lines (unsafeConvert' str)
        -- directive lines: "! @ABL ..." (disabled "!!" ones dropped)
        ablLines = removeCommentsAscl str
        -- content lines: non-blank, not comments, not ABL directives
        contentLines = [ s | s <- rawLines
                           , not (T.isBlank s)
                           , not (T.isPrefixOf "!" s) ]
        snps = contentLines !! 0
        cnt = read (T.unpack $ contentLines !! 1) :: Integer
        pitchLines = drop 2 contentLines
        (pitches0, names1) = unzip $ map parsePitchAscl pitchLines
        dir = parseAsclDirectives ablLines
        -- names: the NOTE_NAMES directive is authoritative; when absent, the
        -- per-pitch comments ("pitch ! name", covering pitch 1..n) supply
        -- them — shifted so the last (pseudo-octave) name becomes pitch class 0.
        hasNoteNames = any (T.isInfixOf "NOTE_NAMES") ablLines
        names1' = case names1 of [] -> []; xs -> last xs : init xs
        names' = if hasNoteNames then names (noteNames dir) else names1'
        REFERENCE_PITCH octave index freq = referencePitch dir -- must be
        -- True only when the file itself declared @ABL REFERENCE_PITCH (the
        -- default directive otherwise pretends to be one)
        hasRef = any (T.isInfixOf "REFERENCE_PITCH") ablLines
        -- Calibrate so the keyboard note playing (octave·12 + index + 24)
        -- sounds exactly @freq@ Hz.  The +24 maps Ableton's display octaves
        -- (C3 = MIDI 60) onto standard MIDI note numbers (A4 = 69 = 440 Hz):
        -- e.g. 12-TET's "3 9 440" → keyboard note 69 = 440.  For non-EDO
        -- scales the index is the scale degree (5-EDO "3 4 440" → note 64 =
        -- degree 4 = 440, giving degree 0 = 252.7 Hz as the file states).
        target = A.calibrate (Ascl "" (-1) snps cnt pitches0 dir hasRef)
            (octave * 12 + index + 24) freq
    in
        case (fromIntegral cnt == length pitchLines) of
            False -> error $ "Mismatch between declared note count and the actual line count in .ascl (" ++
                show cnt ++ " vs " ++ show (length pitchLines) ++ ")"
            True  -> target

-- | Template-Haskell splice: emit a top-level binding like
--   @edo5 :: Ascl@ for the parsed tuning.  Runs at compile time, so a
--   malformed .ascl file fails the build with the parser's error message.
makeAscl :: String -> Ascl -> Q [Dec]
makeAscl name' (Ascl _ low syn cnt pit dir hasRef) = do
    let count = mkName "count"
        name = mkName "scl_name"
        newScl = mkName (firstLow name')
        pitches = mkName "pitches"
        scl_lowest = mkName "scl_lowest"
        scl_count = mkName "scl_count"
        scl_pitches = mkName "scl_pitches"
        scl_synopsis = mkName "scl_synopsis"
        scl_directives = mkName "scl_directives"
        scl_hasRef = mkName "scl_has_reference_pitch"
        -- tunName is Text, so wrap the String literal in T.pack
        qName = qkExpToValD name (AppE (VarE 'T.pack) (LitE (StringL name')))
        qLowest = qkExpToValD scl_lowest (LitE (RationalL (toRational low)))
        qSyn = qkExpToValD scl_synopsis (AppE (VarE 'T.pack) (LitE (StringL (T.unpack syn))))
        qCount = qkExpToValD scl_count (LitE (IntegerL cnt))
        qPitches = qkListDouble scl_pitches pit
        qDirectives = qkExpToValD scl_directives (directiveSetExp dir)
        qHasRef = qkExpToValD scl_hasRef (if hasRef then ConE 'True else ConE 'False)
        -- Ascl tunName lowestFreq synopsis count pitches directives hasReferencePitch
        qScl =
            ValD
                (VarP newScl)
                ( NormalB $ foldl AppE (ConE 'Ascl)
                    [ VarE name, VarE scl_lowest, VarE scl_synopsis
                    , VarE scl_count, VarE scl_pitches, VarE scl_directives
                    , VarE scl_hasRef ] )
                [ qName, qLowest, qSyn, qCount, qPitches, qDirectives, qHasRef ]
    return [qScl]

-- | Turn a parsed directive set into a TH record expression.
directiveSetExp :: A.AsclDirectiveSet -> Exp
directiveSetExp ds = RecConE 'A.DirectiveSet
    [ ('A.referencePitch, asclDirectiveExp (referencePitch ds))
    , ('A.noteNames,     asclDirectiveExp (noteNames ds))
    , ('A.noteRange,     asclDirectiveExp (noteRange ds))
    , ('A.tuningSource,  asclDirectiveExp (tuningSource ds))
    , ('A.sourceLink,    asclDirectiveExp (sourceLink ds))
    ]

asclDirectiveExp :: A.AsclDirective -> Exp
asclDirectiveExp d = case d of
    REFERENCE_PITCH o i f ->
        foldl AppE (ConE 'REFERENCE_PITCH)
            [ LitE (IntegerL o), LitE (IntegerL i), LitE (RationalL (toRational f)) ]
    NOTE_NAMES ns ->
        AppE (ConE 'NOTE_NAMES)
            (ListE [ AppE (VarE 'T.pack) (LitE (StringL (T.unpack n))) | n <- ns ])
    NOTE_RANGE_BY_FREQUENCY a b ->
        foldl AppE (ConE 'NOTE_RANGE_BY_FREQUENCY)
            [ LitE (RationalL (toRational a)), LitE (RationalL (toRational b)) ]
    NOTE_RANGE_BY_INDEX o i o' i' ->
        foldl AppE (ConE 'NOTE_RANGE_BY_INDEX)
            [ LitE (IntegerL o), LitE (IntegerL i), LitE (IntegerL o'), LitE (IntegerL i') ]
    SOURCE s -> AppE (ConE 'SOURCE) (AppE (VarE 'T.pack) (LitE (StringL (T.unpack s))))
    LINK l -> AppE (ConE 'LINK) (AppE (VarE 'T.pack) (LitE (StringL (T.unpack l))))

-- | "EDO5" → "edo5"
firstLow :: String -> String
firstLow cs = map toLower (takeWhile isUpperCase cs) ++ dropWhile isUpperCase cs
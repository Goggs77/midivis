{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Strict #-}
module Midivis.Tuning.Poly where
import Data.Vector.Storable hiding (filter)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Prelude (Double, Int, Bool(..), (+), (-), (*), (/), ($), (==), (||), (&&), otherwise, sqrt, error, abs, filter, (>=), (<=), (<), (>), (.), fromIntegral, show, Foldable, Maybe (..), flip)
import qualified Prelude as P
import Midivis.Util.Math
import Midivis.Tuning.Corpus

(~=) :: Poly -> Poly -> Bool
(~=) p1 p2 = (length p1 == length p2) && and (zipWith (~~) p1 p2)

σ² :: Poly -> S²
σ² pl = 
    let lenD = (fromIntegral $ length pl)
        avg = (foldl1 (+) pl) / lenD
    in foldl1 (+) $ map (\x -> (x - avg) * (x - avg) / lenD) pl

ā :: Poly -> Double
ā pl = foldl (+) 0 pl / (fromIntegral $ length pl)


-- | O(n²) fit, glad we're dealing with such small amount of data
--   alert: length src must be >= 3。
--   Does not identify shifts (>> <<)
--   Exchangable when length src == length tgt.
--   [0, 0.2] -> best
--   (0.2, 0.5] -> good
--   (0.5, 0.75] -> mediocre
--   (0.75, 1] -> bad
fit :: Poly -> Poly -> Double
fit src tgt = fitWith (sMax (length src)) src tgt

-- | Scale-normalization factor of the deviation metric (see 'fit').
--   Supports n in [1, 12]; the corpus never exceeds 8 notes.
sMax :: Int -> Double
sMax n
    | n == 1    = m * 1.0
    | n == 2    = m * 1.5
    | n == 3    = m * ((2.0 + 2.0 * sqrt(2.0)) / 3.0)
    | n == 4    = m * ((3.0 + 2.0 * sqrt(3.0)) / 4.0)
    | n == 5    = m * (8.0 / 5.0)
    | n == 6    = m * ((4.0 + 4.0 * sqrt(2.0)) / 6.0)
    | n == 7    = m * ((5.0 + 2.0 * sqrt(10.0)) / 7.0)
    -- n == 6..7 follow (n-2 + 2*sqrt(2n-4)) / n; keep that for 8..12
    | n <= 12   = m * ((fromIntegral (n - 2) + 2.0 * sqrt (fromIntegral (2 * n - 4))) / fromIntegral n)
    | otherwise = error ("fit: sMax supports at most 12 notes, got " P.++ show n)
  where
    m = 1.0 / 12.0

-- | Minimum absolute distance from x to any element of tgt. Zero-alloc.
minDist :: Double -> Poly -> Double
minDist x tgt = go 0 1.0e300
  where
    m = length tgt
    go i best
      | i >= m    = best
      | otherwise = let d = abs (unsafeIndex tgt i - x)
                    in go (i + 1) (if d < best then d else best)

-- | Zero-allocation 'fit' with the 'sMax' factor precomputed. Hot path:
--   called once per (candidate x inversion) inside 'bestFitTable'.
--   Two passes over the per-note minimum distances, matching the old
--   vector-based formula bit-for-bit (mean first, then the sum of squared
--   deviations / n), so near-ties resolve exactly as before.
fitWith :: Double -> Poly -> Poly -> Double
fitWith sN src tgt =
    (avg + 2.0 * sqrt s2) / sN
  where
    n  = length src
    n' = fromIntegral n
    sumD = goSum 0 0.0
      where
        goSum i acc
          | i >= n    = acc
          | otherwise = goSum (i + 1) (acc + minDist (unsafeIndex src i) tgt)
    avg = sumD / n'
    s2 = goSq 0 0.0
      where
        goSq i acc
          | i >= n    = acc
          | otherwise = let d = minDist (unsafeIndex src i) tgt
                        in goSq (i + 1) (acc + (d - avg) * (d - avg) / n')

-- | 'fit' with '>>' in consideration, note that length src <= length tgt
fit' :: Poly -> Poly -> (Double, Int)
fit' src tgt = 
    let res = P.map (\ d -> (fit src (tgt >> d), d)) [0..length tgt - 1] 
    in P.foldl1 (\(x0, y0) (x1, y1) -> if x0 <= x1 then (x0, y0) else (x1, y1)) res


-- | Deviation difference below which two fits count as a tie.
--   Absorbs double rounding noise from stacked polys (e.g. maj +> min
--   vs. a direct scale12TET list differ by ~1 ulp); must stay far below
--   any real deviation gap (smallest is ~0.02 for 12-TET vs just).
fitEps :: Double
fitEps = 1.0e-9

-- | Is candidate a a better match than b for source s?
--     primary   deviation * (inv + 1)   -- min deviation, penalty on inversion
--     secondary |len tgt - len src|     -- same-size target wins near-zero ties
--     tertiary  inversion               -- smallest inversion first
--   Deviations within 'fitEps' are float noise and count as a tie.
betterFit :: Poly -> (Poly, Double, Int) -> (Poly, Double, Int) -> Bool
betterFit s (t0, d0, i0) (t1, d1, i1)
    | k0 < k1 - fitEps = True
    | k0 > k1 + fitEps = False
    | ld0 < ld1        = True
    | ld0 > ld1        = False
    | i0 < i1          = True
    | otherwise        = False
    where k0  = d0 * (fromIntegral i0 + 1.0)
          k1  = d1 * (fromIntegral i1 + 1.0)
          ld0 = abs (length t0 - length s)
          ld1 = abs (length t1 - length s)

-- | Best-fit match of a poly against a corpus.
--   For every candidate with length >= src it evaluates 'fit'' over all
--   inversions and picks the winner by 'betterFit'.
--   Returns Nothing when no candidate is at least as long as src.
bestFit :: Poly -> [Poly] -> Maybe (Poly, Double, Int)
bestFit src tgts =
    let cands = filter (\ tgt -> length tgt >= length src) tgts
        scored = [ (tgt, delta', inv) | tgt <- cands,
                   let (delta, inv) = fit' src tgt,
                   let delta' = if delta < fitEps then 0.0 else delta ]
    in case scored of
        [] -> Nothing
        fs -> Just (P.foldl1 (\ a b -> if betterFit src b a then b else a) fs)

-- | Fast matching against a precomputed 'CorpusTable' (space for time).
--   Levels:
--     1. exact value lookup (Map, ==)                -> (target, 0, 0)
--     2. 1-cent scan over same-length root positions -> (target, 0, 0)
--     3. exact rotation lookup (Map, ==)             -> (target, 0, inv)
--        -- an input that IS an inversion of a corpus chord equals its
--           precomputed (p >> d) value, so inversions no longer fall
--           through to the fit loop
--     4. 1-cent scan over same-length rotations      -> (target, 0, inv)
--        -- catches frequency inputs that are (quantized) inversions
--     5. fit loop over pre-materialized inversions, bucketed by length,
--        allocation-free deviation metric ('fitWith').
--   Levels 1-2 resolve exactly as before; 3-4 only add faster resolution
--   of inversions that previously fell through to level 5.
bestFitTable :: Poly -> CorpusTable -> Maybe (Poly, Double, Int)
bestFitTable src ct =
    case M.lookup src (ctExact ct) of
        Just _  -> Just (src, 0.0, 0)
        Nothing ->
          case L.find (polyApprox src) (M.findWithDefault [] (length src) (ctByLen ct)) of
            Just k  -> Just (k, 0.0, 0)
            Nothing ->
              case M.lookup src (ctRot ct) of
                Just (k, i) -> Just (k, 0.0, i)
                Nothing ->
                  case bestRotApprox src ct of
                    Just (k, i) -> Just (k, 0.0, i)
                    Nothing     -> fitLayer src ct
  where
    -- Among same-length rotations within 'fepsilog', prefer the smallest
    -- inversion (matches 'betterFit' tertiary tie-break); equal inversions
    -- resolve to the earlier corpus entry (bucket is in corpus order).
    bestRotApprox s ctbl =
        let go best rest = case rest of
              [] -> best
              (r, (k, i)) : xs
                | polyApprox s r -> case best of
                    Nothing            -> go (Just (k, i)) xs
                    Just (_, i0) | i < i0 -> go (Just (k, i)) xs
                                | otherwise -> go best xs
                | otherwise       -> go best xs
        in go Nothing (M.findWithDefault [] (length s) (ctRotByLen ctbl))

    -- The deviation-fit layer. Candidates are grouped by length; we visit
    -- buckets len src, len src+1, ... in that order (the length tie-break
    -- prefers same-size targets) and keep the 'betterFit' winner with a
    -- strict fold -- no intermediate scored list is built.
    fitLayer s ctbl =
        let n      = length s
            sN     = sMax n
            maxLen = P.fst (M.findMax (ctCandsByLen ctbl))
            loop len best
              | len > maxLen = best
              | otherwise =
                  case M.lookup len (ctCandsByLen ctbl) of
                    Nothing -> loop (len + 1) best
                    Just cs -> loop (len + 1) (P.foldl' (step s sN) best cs)
        in loop n Nothing

    step s sN acc (p, invs) =
        let (d, i) = bestInvS s sN invs
            d' = if d < fitEps then 0.0 else d
        in case acc of
            Nothing -> Just (p, d', i)
            Just a  -> if betterFit s (p, d', i) a then Just (p, d', i) else Just a

    bestInvS s sN invs = case invs of
        [] -> error "bestInvS: empty inversion list"
        (inv0, d0) : rest -> L.foldl' step (fitWith sN s inv0, d0) rest
      where
        step (x0, y0) (inv, d) =
            let x1 = fitWith sN s inv
            in if x0 <= x1 then (x0, y0) else (x1, d)

-- | Takes (preprocessed Poly) and evaluate differences against the
--   12-TET corpus. Outputs best-match Poly, delta and inversion.
fit12TET :: Poly -> (Poly, Double, Int)
fit12TET src =
    case bestFit src all12TET of
        Just r  -> r
        Nothing -> error "fit12TET: no 12-TET candidate at least as long as src"

-- | Best match against the full corpus (12-TET + just intervals +
--   just chords + octave doublings), using the precomputed table.
fitAll :: Poly -> Maybe (Poly, Double, Int)
fitAll = flip bestFitTable allCorpusTable

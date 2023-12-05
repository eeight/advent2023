import qualified Data.Map as M
import Data.List(nub, foldl',sort)

parseRanges :: [String] -> [(Integer, Integer, Integer)]
parseRanges = map parseEntry where
    parseEntry l = let
        [dst, src, len] = map read $ words l
        in (src, dst, len)

mapRange (b, l) (src, dst, len) = let
    b' = max b src
    e' = min (b + l) (src + len)
    in if e' <= b'
        then []
        else [(b' - src + dst, e' - b')]

dropRange (b, l) (src, _, len) = let
    b' = max b src
    e' = min (b + l) (src + len)
    in if e' <= b'
        then [(b, l)]
        else filter (\(_, l) -> l > 0) [(b, min l (src - b)), (src + len, b + l - src - len)]

lookupRanges :: [(Integer, Integer)] -> [(Integer, Integer, Integer)] -> [(Integer, Integer)]
lookupRanges xs ys = concatMap lookupRange xs where
    lookupRange x = concatMap (mapRange x) ys ++ foldl' (\xs y -> concatMap (flip dropRange y) xs) [x] ys

parseSeedRanges :: [String] -> [(Integer, Integer)]
parseSeedRanges = go . map read where
    go [] = []
    go (b:l:xs) = (b, l):go xs

solve input = let
    s:_:ls = lines input
    seeds = sort . parseSeedRanges . tail . words $ s
    lll = sum . map snd $ seeds

    go :: [(Integer, Integer)] -> [String] -> [(Integer, Integer)]
    go rs [] = rs
    go rs text = let
        (_:ranges, text_tail) = break (== "") text
        rangeMap = parseRanges ranges
        rs' = lookupRanges rs rangeMap
        in go rs' (if not (null text_tail) then tail text_tail else [])

    in fst . minimum $ go seeds ls

main = solve <$> getContents >>= print

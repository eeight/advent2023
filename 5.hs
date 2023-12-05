import qualified Data.Map as M
import Data.List(nub)
import Debug.Trace

parseRanges :: [String] -> [(Integer, Integer, Integer)]
parseRanges = map parseEntry where
    parseEntry l = let
        [dst, src, len] = map read $ words l
        in (src, dst, len)

lookupRange :: Integer -> [(Integer, Integer, Integer)] -> Integer
lookupRange x [] = x
lookupRange x ((src, dst, len):ys)
    | x >= src && x <= src + len = x - src + dst
    | otherwise = lookupRange x ys

solve input = let
    s:_:ls = lines input
    seeds = map read . tail . words $ s

    go ns [] = ns
    go ns text = let
        (_:ranges, text_tail) = break (== "") text
        rangeMap = traceShowId $ parseRanges ranges
        ns' = map (flip lookupRange rangeMap) ns
        in go ns' (if not (null text_tail) then tail text_tail else [])

    in minimum $ go seeds ls

main = solve <$> getContents >>= print

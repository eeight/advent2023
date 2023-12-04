import Data.List.Split(splitOn)
import Data.List(foldl')
import Data.Map((!))
import qualified Data.Map as M

parse :: String -> [([Int], [Int])]
parse = map p . lines where
    p l = let
        [_, l'] = splitOn ": " l
        [w, o] = splitOn " | " l'
        in (map read . words $ w, map read . words $ o)

solve cs = let
    score (w, o) = length . filter (`elem` w) $ o
    n = length cs

    add :: Integer -> Int -> M.Map Int Integer -> M.Map Int Integer
    add m i = M.update (pure . (+ m)) i

    go counts _ [] = counts
    go counts i (s:ss) = let
        m = counts ! i
        counts' = foldl' (.) id [add m (i + j) | j <- [1..s]] counts
        in go counts' (i + 1) ss

    counts = go (M.fromList [(i, 1) | i <- [1..n]]) 1 $ map score cs
    in sum [counts ! i | i <- [1..n]]

main = (solve . parse) <$> getContents >>= print

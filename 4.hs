import Data.List.Split(splitOn)

parse = map p . lines where
    p l = let
        [_, l'] = splitOn ": " l
        [w, o] = splitOn " | " l'
        in (map read . words $ w, map read . words $ o)

solve :: [([Int], [Int])] -> Integer
solve = sum . map score where
    score (w, o) = let
        n = length . filter (`elem` w) $ o
        in case n of
            0 -> 0
            n' -> 2 ^ (n  - 1)

main = (solve . parse) <$> getContents >>= print

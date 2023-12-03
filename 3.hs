import Data.Array
import Data.Char(isDigit)

parse :: String -> Array (Int, Int) Char
parse input = let
    ls = lines input
    nrows = length ls
    ncols = length $ head ls
    fillerRow = replicate (ncols + 2) '.'
    field = listArray ((0, 0), (nrows + 1, ncols + 1)) $ concat (
        [fillerRow] ++ (map (\r -> "." ++ r ++ ".") ls) ++ [fillerRow])
    in field

solve :: Array (Int, Int) Char -> Int
solve field = go (1, 1) 0 where
    (_, (m, n)) = bounds field
    go :: (Int, Int) -> Int -> Int
    go (i, j) acc
        | (i, j) == (m - 1, n - 1) = acc
        | not . isDigit $ field ! (i, j) = let
            (i', j') = if (j + 1) < n then (i, j + 1) else (i + 1, 1)
            in go (i', j') acc
        | otherwise = let
            endJ :: Int
            endJ = last $ takeWhile (\j' -> isDigit (field ! (i, j'))) [j..]
            partN :: Int
            partN = read $ [field ! (i, j') | j' <- [j..endJ]]
            toTest :: [(Int, Int)]
            toTest = concat [
                [(i - 1, jj) | jj <- [j - 1 .. endJ + 1]],
                [(i + 1, jj) | jj <- [j - 1 .. endJ + 1]],
                [(i, j - 1), (i, endJ + 1)]]
            mustAdd = any (\c -> field ! c /= '.') toTest
            acc' = if mustAdd then acc + partN else acc
            in go (i, endJ + 1) acc'

main = (solve . parse) <$> getContents >>= print

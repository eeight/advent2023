import Data.Array
import Data.Char(isDigit)
import Data.List

parse :: String -> Array (Int, Int) Char
parse input = let
    ls = lines input
    nrows = length ls
    ncols = length $ head ls
    fillerRow = replicate (ncols + 2) '.'
    field = listArray ((0, 0), (nrows + 1, ncols + 1)) $ concat (
        [fillerRow] ++ (map (\r -> "." ++ r ++ ".") ls) ++ [fillerRow])
    in field

solve field = let
    (_, (m, n)) = bounds field
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
            in go (i, endJ + 1) $ acc ++ [(c, partN) | c <- toTest, field ! c == '*']
    gears = go (1, 1) []
    gears' :: [[Integer]]
    gears' = map (map (fromIntegral . snd)) . filter ((== 2) . length) . groupBy (\x y -> fst x == fst y) $ sort gears
    in sum . map product $ gears'

main = (solve . parse) <$> getContents >>= print

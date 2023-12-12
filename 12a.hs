import Data.List.Split(splitOn, wordsBy)
import qualified Data.Array as A
import Data.List(intercalate)

parse = map parseL . lines where
    parseL :: String -> (String, [Int])
    parseL l = let
        [code, counts] = words l
        counts' = map read $ splitOn "," counts
        code' = intercalate "?" $ replicate 5 code
        counts'' = concat $ replicate 5 counts'
        in (code', counts'')

solve = sum . map countWays where
    countWays (code, counts) = let
        lcode = length code
        lcounts = length counts
        codeA = A.listArray (0, lcode - 1) code
        countsA = A.listArray (0, lcounts - 1) counts
        results = A.listArray ((0, 0), (lcode, lcounts)) [countFor n m | n <- [0..lcode], m <- [0..lcounts]]
        countFor :: Int -> Int -> Integer
        countFor n m
            | length (filter (== '#') $ drop n code) > sum (drop m counts) = 0
            | length (filter (`elem` "?#") $ drop n code) < sum (drop m counts) = 0
            | m == lcounts = 1
            | otherwise = let
                size = countsA A.! m
                block = take size (drop n code)
                end = if (n + size) < lcode then codeA A.! (n + size) else '.'
                canPlace = all (`elem` "#?") block && end /= '#'
                resultIfPlaced = if (n + size + 1) < lcode then results A.! (n + size + 1, m + 1) else results A.! (lcode, m + 1)
                resultIfSkipped = results A.! (n + 1, m)
                in case (codeA A.! n, canPlace) of
                    ('#', True) -> resultIfPlaced
                    ('#', False) -> 0
                    ('.', _) -> resultIfSkipped
                    ('?', True) -> resultIfPlaced + resultIfSkipped
                    ('?', False) -> resultIfSkipped
        in results A.! (0, 0)

main = (solve . parse) <$> getContents >>= print

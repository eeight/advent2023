import Data.List.Split(splitOn)
import Data.List(transpose, nub)

parse = splitOn [""] . lines

replaces p f = go p [] f where
    go [] rest _ = []
    go (x:xs) rest f = map (\y -> (reverse rest ++ (y:xs))) (f x) ++ go xs (x:rest) f

solve' p = let
    hasSymmetry cs i = all id $ zipWith (==) (reverse $ take i cs) (drop i cs)
    rowSymmetries = filter (hasSymmetry p) [1..length p - 1]
    colSymmetries = filter (hasSymmetry $ transpose p) [1..length (head p) - 1]
    in (colSymmetries, rowSymmetries)

solve p = let
    (cs, rs) = solve' p
    ps = replaces p (\r -> replaces r (\x -> filter (/= x) ".#"))
    rs' = sum $ nub $ [sum $ filter (not . (`elem` rs)) ys | (xs, ys) <- map solve' ps]
    cs' = sum $ nub $ [sum $ filter (not . (`elem` cs)) xs | (xs, ys) <- map solve' ps]
    in cs' + rs'* 100

main = (sum . map solve . parse) <$> getContents >>= print

import Data.List.Split(splitOn)
import Data.List(transpose)

parse = splitOn [""] . lines

solve p = let
    hasSymmetry cs i = all id $ zipWith (==) (reverse $ take i cs) (drop i cs)
    rowSymmetries = filter (hasSymmetry p) [1..length p - 1]
    colSymmetries = filter (hasSymmetry $ transpose p) [1..length (head p) - 1]
    in sum colSymmetries + 100 * sum rowSymmetries

main = (sum . map solve . parse) <$> getContents >>= print

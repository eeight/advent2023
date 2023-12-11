import qualified Data.Array as A
import qualified Data.Set as S
import Data.List(tails)

parse t = let
    l = lines t
    n = length $ l
    m = length $ head l
    in A.listArray ((0, 0), (n - 1, m - 1)) $ concat $ lines t


solve f = let
    (_, (n1, m1)) = A.bounds f
    isEmptyRow i = all (== '.') [f A.! (i, j) | j <- [0..m1]]
    isEmptyCol j = all (== '.') [f A.! (i, j) | i <- [0..n1]]
    emptyRows = S.fromList $ filter isEmptyRow [0..n1]
    emptyCols = S.fromList $ filter isEmptyCol [0..m1]
    expand s i = i + (S.size $ fst $ S.split i s)

    galaxyCoords = map (\(y, x) -> (expand emptyRows y, expand emptyCols x)) . filter ((== '#') . (f A.!)) $ A.indices f

    in sum [abs (x1 - x2) + abs (y1 - y2) | ((y1, x1):gs) <- tails galaxyCoords, (y2, x2) <- gs]

main = (solve . parse) <$> getContents >>= print

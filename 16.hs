import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S

data Dir = U | D | L | R deriving (Eq, Ord, Show)


dir U = (-1, 0)
dir D = (1, 0)
dir L = (0, -1)
dir R = (0, 1)

undir (-1, 0) = U
undir (1, 0)  = D
undir (0, -1) = L
undir (0, 1)  = R


reflect '.' c = [c]

reflect '/' U = [R]
reflect '/' D = [L]
reflect '/' L = [D]
reflect '/' R = [U]

reflect '\\' U = [L]
reflect '\\' D = [R]
reflect '\\' L = [U]
reflect '\\' R = [D]

reflect '-' U = [L, R]
reflect '-' D = [L, R]
reflect '-' L = [L]
reflect '-' R = [R]

reflect '|' U = [U]
reflect '|' D = [D]
reflect '|' L = [U, D]
reflect '|' R = [U, D]


parse t = let
    ls = lines t
    n = length ls
    m = length $ head ls
    in A.listArray ((0, 0), (n - 1, m - 1)) $ concat ls


solve f = let
    go visited [] = visited
    go visited ts = let
        next (y, x) d = let
            (dy, dx) = dir d
            c' :: (Int, Int)
            c' = (y + dy, x + dx)
            in if A.inRange (A.bounds f) c'
                then filter (not . (`S.member` visited)) $ map (c',) $ reflect (f A.! c') d
                else []

        ts' = concatMap (uncurry next) ts
        in go (visited `S.union` (S.fromList ts')) ts'
    visited = go S.empty [((0, -1), R)]
    in S.size . S.fromList . map fst $ S.toList visited

main = (solve . parse) <$> getContents >>= print

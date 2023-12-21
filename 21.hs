import qualified Data.Array as A
import qualified Data.Map as M
import Data.List(foldl', nub)
import qualified Data.Set as S

parse t = let
    ls = lines t
    n = length ls
    m = length $ head ls
    in A.listArray ((0, 0), (n - 1, m - 1)) $ concat ls

bfs f s = go [s] 0 M.empty where
    bs = A.bounds f
    go :: [(Int, Int)] -> Int -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
    go xs n d = let
        d' = foldl' (\m x -> M.insertWith (flip const) x n m) d xs
        xs' = S.toList . S.fromList . filter isValid . concatMap neighbors $ xs where
            neighbors (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
            isValid c = A.inRange bs c && f A.! c /= '#'
        in if M.size d' > M.size d
            then go xs' (n + 1) d'
            else d

solve f = let
    spos = fst . head . filter ((== 'S') . snd) $ A.assocs f
    ds = M.elems $ bfs f spos
    in length $ filter (\d -> d <= 64 && d `mod` 2 == 0) ds

main = solve . parse <$> getContents >>= print


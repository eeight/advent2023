import qualified Data.Array as A
import qualified Data.Map as M
import Data.List(foldl', nub)
import qualified Data.Set as S

parse t = let
    ls = lines t
    n = length ls
    m = length $ head ls
    in A.listArray ((0, 0), (n - 1, m - 1)) $ concat ls

bfs f maxD s = go s 0 M.empty where
    bs = A.bounds f
    go :: [(Int, Int)] -> Int -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
    go xs n d = let
        d' = foldl' (\m x -> M.insertWith (flip const) x n m) d xs
        xs' = S.toList . S.fromList . filter isValid . concatMap neighbors $ xs where
            neighbors (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
            isValid c = A.inRange bs c && f A.! c /= '#' && not (M.member c d')
        in if M.size d' > M.size d && n < maxD
            then go xs' (n + 1) d'
            else d'

solve f = let
    target = 26501365
    step = 131
    mid = step `div` 2

    (fullSteps, rest) = target `divMod` step
    mm' = target `mod` 2

    spos = fst . head . filter ((== 'S') . snd) $ A.assocs f

    boardS n mm s = let
        ds = M.elems $ bfs f n [s]
        in length $ filter (\d -> d <= n && d `mod` 2 == mm) ds

    boardS' n = boardS n (n `mod` 2)

    b0, b1 :: Integer
    b0 = fromIntegral $ boardS 1000 mm' spos
    b1 = fromIntegral $ boardS 1000 (1 - mm') spos

    fs = fromIntegral fullSteps
    fs0 = (fs `div` 2) ^ 2
    fs1 = (fs - 1) * fs `div` 2 - fs0
    corners = [(0, 0), (step - 1, 0), (0, step - 1), (step - 1, step - 1)]
    caps = fromIntegral $ sum $ map (boardS' (rest + mid)) [(0, mid), (step - 1, mid), (mid, 0), (mid, step - 1)]
    smallSides = fromIntegral $ sum $ map (boardS' (rest - 1)) corners
    bigSides = fromIntegral $ sum $ map (boardS' (rest - 1 + step)) corners
    in b0 + 4 * (fs1 * b0 + fs0 * b1) + caps + fs * smallSides + (fs - 1) * bigSides

main = solve . parse <$> getContents >>= print


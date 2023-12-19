import qualified Data.Set as S
import Data.Function(on)
import Data.List(sortBy, foldl', tails)

data D = U | D | L | R deriving (Read, Show, Eq)

parse :: String -> [(D, Int)]
parse = map p . lines where
    p l = let
        [_, _, h] = words l
        h' = drop 2 h
        n = take 5 h'
        d = case head (drop 5 h') of
            '0' -> R
            '1' -> D
            '2' -> L
            '3' -> U
        in (d, read $ ("0x" ++ n))

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p' (x':xs') = (x' : ys') : zs'
  where
    (ys',zs') = go p' x' xs'
    go p z (x:xs)
      | p z x = (x : ys, zs)
      | otherwise = ([], (x : ys) : zs)
      where (ys,zs) = go p x xs
    go _ _ [] = ([], [])

xor = foldl' (/=) False

makeSegments isCw cmds = go (fst $ last cmds) (0, 0) cmds where
    go _ _ [] = []
    go prevD (y, x) ((d, n):cs) =  let
        (dx, dy) = case d of
            L -> (-1, 0)
            R -> (1, 0)
            U -> (0, -1)
            D -> (0, 1)
        y' = y + dy * n
        x' = x + dx * n
        nextD = if null cs then fst (head cmds) else fst (head cs)
        shortBegin = xor [isCw, d == D, prevD == L]
        shortEnd =   xor [isCw, d == D, nextD == R]
        hh = case d of
            U -> [(x + (1 - fromEnum isCw), y' + fromEnum shortEnd, y + 1 - fromEnum shortBegin)]
            D -> [(x + fromEnum isCw, y + fromEnum shortBegin, y' + 1 - fromEnum shortEnd)]
            _ -> []
        in hh ++ go d (y', x') cs

count :: S.Set Int -> Integer
count = go 0 . S.toList where
    go acc [] = acc
    go acc (y1:y2:ys) = go (acc + fromIntegral y2 - fromIntegral y1) ys

scan :: [[(Int, S.Set Int -> S.Set Int)]] -> Integer
scan = go 0 0 S.empty (-100000000) where
    go acc _ _ _ [] = acc
    go acc rowSize set y (row:rs) = let
        y' = fst $ head row
        set' = foldl' (flip ($)) set $ map snd row
        rowSize' = count set'
        acc' = acc + rowSize * fromIntegral (y' - y)
        in go acc' rowSize' set' y' rs

fullScan segments = let
    events = sortBy (compare `on` fst) $ concatMap (\(x, y1, y2) -> [(y1, S.insert x), (y2, S.delete x)]) segments
    rows = groupBy (\a b -> fst a == fst b) events
    in scan rows

solve cmds = maximum [fullScan $ makeSegments b cmds | b <- [False, True]]

main = solve . parse <$> getContents >>= print

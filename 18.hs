import qualified Data.Set as S
import Data.Function(on)
import Debug.Trace

data D = U | D | L | R deriving (Read, Show)

parse :: String -> [(D, Int)]
parse = map p . lines where
    p l = let [d, n, _] = words l in (read d, read n)


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

solve cmds = let
    go f _ [] = f
    go f (x, y) ((d, n):cs) = let
        (dx, dy) = case d of
            L -> (-1, 0)
            R -> (1, 0)
            U -> (0, -1)
            D -> (0, 1)
        newP = [(x + dx*i, y + dy*i) | i <- [1..n]]
        in go (S.union f (S.fromList newP)) (x + dx*n, y + dy*n) cs

    borderS = go (S.singleton (0, 0)) (0, 0) cmds
    border = S.toList borderS
    rows = groupBy ((==) `on` fst) border
    countInterior row = let
        x = fst $ head row
        ys = map snd row
        intersects = groupBy (\a b -> a + 1 == b) ys

        go :: Int -> Int -> Bool -> [[Int]] -> Int
        go acc _ _ []  = acc
        go acc y isInside (ys:yss) = let
            firstY = head ys
            lastY = last ys
            isInside' = isInside /= (
                ((x - 1, firstY) `S.member` borderS && (x + 1, lastY) `S.member` borderS) ||
                ((x + 1, firstY) `S.member` borderS && (x - 1, lastY) `S.member` borderS))
            acc' = if isInside then traceShow ("add", firstY, y) $ acc + (firstY - y - 1) else acc
            in go acc' lastY isInside' yss


        in traceShow intersects $ go 0 (-1000) False intersects
    in length border + sum (map countInterior rows)

main = solve . parse <$> getContents >>= print

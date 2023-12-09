import Data.List.Split(splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map((!))
import Data.Function(on)
import qualified Data.Array as A
import Data.List(sortBy)

parse i = let
    p:_:ns = lines i
    parseN k = let
        [n, opts] = splitOn " = " k
        [l, r] = splitOn ", " opts
        in (n, (tail l, take 3 r))

    nodes = sortBy (compare `on` (reverse . fst)) $ map parseN ns
    nodeToN = M.fromList $ zip (map fst nodes) [0..]

    groupSize = length $ filter ((== 'A') . last) $ map fst nodes

    nextArray = A.listArray (0, length nodes - 1) $ [(nodeToN ! l, nodeToN ! r) | (_, (l, r)) <- nodes]

    in (p, groupSize, nextArray)

crtSolve ((a, n):ans) = go (fromIntegral a) (fromIntegral n) ans where
    go :: Integer -> Integer -> [(Integer, Integer)] -> Integer
    go a n [] = a
    go a n ((b, m):ans) = go (head $ dropWhile ((/= b) . (`mod` m)) [a,a + n ..]) (lcm n m) ans

solve (p, groupSize, ns) = let
    next :: Char -> Int -> Int
    next 'L' !n = fst $ ns A.! n
    next 'R' !n = snd $ ns A.! n

    pn = length p
    pa = A.listArray (0, pn) p

    startingNodes = [0..groupSize - 1]
    endThreshold = snd (A.bounds ns) + 1 - groupSize

    getCycle i start = go i start S.empty where
        go i offset states
            | S.member (i, offset) states = S.size states
            | otherwise = go (next (pa A.! offset) i) ((offset + 1) `mod` pn) $ S.insert (i, offset) states

    firstStepN = maximum $ [getCycle i 0 | i <- startingNodes]
    states i j = iterate (\(n, o) -> (next (pa A.! o) n, (o + 1) `mod` pn)) (i, j)
    ns' = [fst (states i 0  !! firstStepN) | i <- startingNodes]

    cycles = [getCycle n (firstStepN `mod` pn) | n <- ns']
    endPositions = [length . takeWhile ((< endThreshold) . fst) $ states n (firstStepN `mod` pn) | n <- ns']

    nextStepN = crtSolve $ zip (map fromIntegral endPositions) (map fromIntegral cycles)

    in fromIntegral firstStepN + nextStepN


main = (solve . parse) <$> getContents >>= print

import Data.List.Split(splitOn)
import Data.List(tails, sortBy, foldl')
import Data.Function(on)
import qualified Data.Array as A
import Control.Exception(assert)
import qualified Data.Map as M

type C = (Int, Int, Int)
type B = (C, C)

x_ (x, y, z) = x
y_ (x, y, z) = y
z_ (x, y, z) = z

minx = x_ . fst
maxx = x_ . snd
miny = y_ . fst
maxy = y_ . snd
minz = z_ . fst
maxz = z_ . snd

parse = map p . lines where
    p :: String -> B
    p l = let
        cs t = let [x,y,z] = map read $ splitOn "," t in (x, y, z)
        [a, b] = map cs $ splitOn "~" l
        in assert (x_ a <= x_ b && y_ a <= y_ b && z_ a <= z_ b) $ (a, b)

sortz = sortBy (compare `on` minz)

dropBs :: [B] -> [B]
dropBs bs = let
    bs' = sortz bs

    dropOne :: A.Array (Int, Int) Int -> B -> B
    dropOne hs ((x1, y1, z1), (x2, y2, z2)) = let
        h = maximum $ [hs A.! (x, y) | x <- [x1..x2], y <- [y1..y2]]
        d = min z1 z2 - h - 1
        in ((x1, y1, z1 - d), (x2, y2, z2 - d))

    update :: A.Array (Int, Int) Int -> B -> A.Array (Int, Int) Int
    update hs ((x1, y1, z1), (x2, y2, z2)) = let
        z = max z1 z2
        in hs A.// [((x, y), z) | x <- [x1..x2], y <- [y1..y2]]

    go :: A.Array (Int, Int) Int -> [B] -> [B]
    go _ [] = []
    go hs (b:bs) = let
        b' = dropOne hs b
        in b':go (update hs b') bs

    xS = maximum $ map maxx bs'
    yS = maximum $ map maxy bs'
    ground = A.listArray ((0, 0), (xS, yS)) $ replicate ((xS + 1) * (yS + 1)) 0

    in go ground bs'

onTop a b = ir (minx a) (maxx a) (minx b) (maxx b) && ir (miny a) (maxy a) (miny b) (maxy b) where
    ir a b c d = max a c <= min b d

supports :: (Int, B) -> [(Int, B)] -> [(Int, Int)]
supports (n, b) bs = let
    h = maxz b
    ms = map fst . filter (onTop b . snd) . dropWhile ((/= h + 1) . minz . snd) $ takeWhile ((<= h + 1) . minz . snd) bs
    in map (n,) ms

solve bs = let
    maxn = length bs' - 1
    bs' = sortz $ dropBs bs
    supportP :: [(Int, Int)]
    supportP = concat [supports x xs | x:xs <- tails $ zip [0..] bs']
    countM = M.fromListWith (+) $ map ((,1) . snd) supportP
    supportA = A.accumArray (flip (:)) [] (0, maxn) supportP

    runDestroy n = go 0 [n] countM where
        go acc [] _ = acc
        go acc (x:xs) countM = let
            ss = supportA A.! x
            countM' = foldl' (flip $ M.alter $ fmap $ subtract 1) countM ss
            ys = filter ((== 1) . (countM M.!)) ss
            in go (acc + 1) (ys ++ xs) countM'

    in sum (map runDestroy [0..maxn]) - maxn - 1

main = solve . parse <$> getContents >>= print


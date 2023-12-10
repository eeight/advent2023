import qualified Data.Array as A
import Debug.Trace

parse :: String -> A.Array (Int, Int) Char
parse t = let
    l = lines t
    n = length $ l
    m = length $ head l
    in A.listArray ((0, 0), (n - 1, m - 1)) $ concat $ lines t

solve f = let
    [(sy, sx)] = map fst . filter ((== 'S') . snd) $ A.assocs f
    getf c
        | A.inRange (A.bounds f) c = f A.! c
        | otherwise = '.'

    neighbors (y, x) = let
        n = (y - 1, x)
        s = (y + 1, x)
        e = (y, x + 1)
        w = (y, x - 1)
        in case getf (y, x) of
            '|' -> [n, s]
            '-' -> [e, w]
            'L' -> [n, e]
            'J' -> [n, w]
            '7' -> [s, w]
            'F' -> [s, e]
            '.' -> []

    next prev c = head . filter (/= prev) $ neighbors c
    [n1, n2] = filter (elem (sy, sx) . neighbors) $ filter (/= (sy, sx)) [(y, x) | x <- [sx - 1 .. sx + 1], y <- [sy - 1 .. sy + 1]]

    loop = takeWhile ((/= (sy, sx)) . snd) $ iterate (\(c1, c2) -> (c2, next c1 c2)) ((sy, sx), n1)

    in (length loop + 1) `div` 2

main = (solve . parse) <$> getContents >>= print

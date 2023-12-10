import qualified Data.Array as A
import qualified Data.Set as S
import Data.List(sort)

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

    neighbors' (y, x) c  = let
        n = (y - 1, x)
        s = (y + 1, x)
        e = (y, x + 1)
        w = (y, x - 1)
        in case c of
            '|' -> [n, s]
            '-' -> [e, w]
            'L' -> [n, e]
            'J' -> [n, w]
            '7' -> [s, w]
            'F' -> [s, e]
            '.' -> []

    neighbors (y, x) = neighbors' (y, x) (getf (y, x))

    next prev c = head . filter (/= prev) $ neighbors c
    [n1, n2] = filter (elem (sy, sx) . neighbors) $ filter (/= (sy, sx)) [(y, x) | x <- [sx - 1 .. sx + 1], y <- [sy - 1 .. sy + 1]]

    [sPipe] = filter (\p -> sort (neighbors' (sy, sx) p) == sort ([n1, n2])) "|-LJ7F"

    getf2 (y, x)
        | (y, x) == (sy, sx) = sPipe
        | otherwise = getf (y, x)

    loop' = map snd $ takeWhile ((/= (sy, sx)) . snd) $ iterate (\(c1, c2) -> (c2, next c1 c2)) ((sy, sx), n1)
    loop = (sy, sx) : loop'

    loopS = S.fromList loop
    isLoop = (`S.member` loopS)

    nonLoop = filter (not . isLoop) $ A.indices f

    countIntersects :: String -> Int
    countIntersects = go 0 ' ' where
        go acc _ [] = acc
        go acc state (c:cs) = case c of
            '|' -> go (acc + 1) ' ' cs
            'L' -> go acc 'u' cs
            'J' -> go (acc + fromEnum (state == 'd')) ' ' cs
            '7' -> go (acc + fromEnum (state == 'u')) ' ' cs
            'F' -> go acc 'd' cs
            '-' -> go acc state cs

    isInside (sy, sx) = (== 1) . (`mod` 2) . countIntersects . map getf2 . filter isLoop $ [(sy, x) | x <- [0..sx - 1]]

    in length $ filter isInside nonLoop

main = (solve . parse) <$> getContents >>= print

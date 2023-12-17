import qualified Data.Array as A
import qualified Data.HashSet as S
import Data.Graph.AStar
import Data.Char(ord)

parse t = let
    ls = lines t
    n = length ls
    m = length $ head ls
    in A.listArray ((0, 0), (n - 1, m - 1)) $ concat ls

solve f = let
    bs = A.bounds f

    isValid (_, _, n, y, x) = n <= 10 && A.inRange bs (y, x)

    outEdges (dy, dx, n, y, x) = let
        cont = (dy, dx, n + 1, y + dy, x + dx)
        turns = [
            (-dx, dy, 1, y - dx, x + dy),
            (dx, -dy, 1, y + dx, x - dy)]
        outs = if n >= 4 then cont:turns else [cont]
        in S.fromList $ filter isValid outs

    cost :: (Int, Int, Int, Int, Int) -> Int
    cost (_, _, _, y, x) = ord (f A.! (y, x)) - ord '0'
    dist _ b = cost b
    isGoal (_, _, n, y, x) = n >= 4 && (y, x) == snd bs

    runSearch start = let
        Just path = aStar outEdges dist (const 0) isGoal start
        in sum $ map cost path
        -- in map (\(_, _, _, y, x) -> (y, x)) path

    in minimum $ map runSearch [
        (0, 1, 0, 0, 0),
        (1, 0, 0, 0, 0)]

main = solve . parse <$> getContents >>= print

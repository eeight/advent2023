import Data.List(tails)
import Data.List.Split(splitOn)
import Control.Exception(assert)

type C = (Double, Double)
eps = 1e-8

parse :: String -> [(C, C)]
parse = map p . lines where
    p l = let
        pp v = let [x, y, _] = map read $ splitOn ", " v in (x, y)
        [p, v] = map pp $ splitOn " @ " l
        in (p, v)

line (x, y) (dx, dy) = let
    a = -dy
    b = dx
    c = -a * x -b * y
    z = sqrt $ a*a + b*b
    z' = if abs z > eps then z else 1.0
    in (a / z', b / z', c / z')

det a b c d = a * d - b * c

intersect (p1, v1) (p2, v2) = let
    (ma, mb, mc) = line p1 v1
    (na, nb, nc) = line p2 v2
    zn = det ma mb na nb
    in if abs zn < eps
        then Nothing
        else let
            x = -(det mc mb nc nb) / zn
            y = -(det ma mc na nc) / zn
            in Just (x, y)

solve ps = let
    isForward (x, y) ((px, py), (dx, dy)) =
        if abs dx > eps
            then signum (x - px) == signum dx
            else signum (y - py) == signum dy

    inBounds (x, y) = inBounds' x && inBounds' y where
        inBounds' x = x >= 200000000000000 && x <= 400000000000000

    doCross pv1 pv2 = case intersect pv1 pv2 of
        Nothing -> False
        Just p -> inBounds p && isForward p pv1 && isForward p pv2

    in sum $ [fromEnum $ doCross pv1 pv2 | pv1:pps <- tails ps, pv2 <- pps]

main = solve . parse <$> getContents >>= print

import Data.Function(on)
import Data.List(sort)
import Data.List.Split(splitOn)
import Control.Exception(assert)

type C = (Int, Int, Int)

parse :: String -> [(C, C)]
parse = map p . lines where
    p l = let
        pp v = let [x, y, z] = map read $ splitOn ", " v in (x, y, z)
        [p, v] = map pp $ splitOn " @ " l
        in (p, v)

sub (a1, a2, a3) (b1, b2, b3) = (a1 - b1, a2 - b2, a3 - b3)
add (a1, a2, a3) (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)
mul k (a1, a2, a3) = (k * a1, k * a2, k * a3)

x_ (x, y, z) = x
y_ (x, y, z) = y
z_ (x, y, z) = z

safeDiv x y = let (d, m) = x `divMod`y in assert (m == 0) d

solve hs = let
    -- There are two hailstones with matching x coords and x velocities
    hs' = sort hs
    ((x0, _, _), (dx0, _, _)) = fst $ head $ filter (uncurry ((==) `on` (x_ . fst))) $ zip hs' (tail hs')

    [(p1, d1), (p2, d2)] = take 2 $ hs
    t1 = ((x0 - x_ p1) `safeDiv` (x_ d1 - dx0))
    t2 = ((x0 - x_ p2) `safeDiv` (x_ d2 - dx0))
    dt = t2 - t1

    c1 = p1 `add` mul t1 d1
    c2 = p2 `add` mul t2 d2
    dc = c2 `sub` c1

    dy0 = (y_ dc `safeDiv` dt)
    dz0 = (z_ dc `safeDiv` dt)

    y0 = y_ c1 - dy0 * t1
    z0 = z_ c1 - dz0 * t1

    in (x0 + y0 + z0)

main = solve . parse <$> getContents >>= print

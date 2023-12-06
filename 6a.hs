parse :: String -> (Int, Int)
parse l = let
    [t, d] = map (read . concat . tail . words) . lines $ l
    in (t, d)

solveQ :: Double -> Double -> (Double, Double)
solveQ b c = let
    d = sqrt $ b^2 + 4 * c
    ma2 = -0.5
    in (ma2 * (-b + d), ma2 * (-b - d))

solve :: (Int, Int) -> Integer
solve = fromIntegral . go where
    go (t, d) = let
        (x1, x2) = solveQ (fromIntegral t) (fromIntegral $ -d)
        (y1, y2) = (max 0 $ ceiling $ x1 + 1e-9, floor $ x2 - 1e-9)
        in y2 - y1 + 1

main = (solve . parse) <$> getContents >>= print

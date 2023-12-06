import Control.Exception(assert)
import Debug.Trace

parse :: String -> [(Int, Int)]
parse l = let
    [t, d] = map (map read . tail . words) . lines $ l
    in zip t d

solveQ :: Double -> Double -> Double -> (Double, Double)
solveQ a b c = let
    d = sqrt $ b^2 - 4 * a * c
    ma2 = 1.0 / (2 * a)
    in (ma2 * (-b + d), ma2 * (-b - d))

solve :: [(Int, Int)] -> Integer
solve = product . map (fromIntegral . go) where
    go (t, d) = let
        (x1, x2) = solveQ (-1) (fromIntegral t) (fromIntegral $ -d)
        (y1, y2) = (max 0 $ ceiling $ x1 + 1e-9, floor $ x2 - 1e-9)
        in y2 - y1 + 1

main = (solve . parse) <$> getContents >>= print

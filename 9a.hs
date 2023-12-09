parse :: String -> [[Integer]]
parse = map (map read . words) . lines

solve = sum . map next where
    next ns
        | all (== 0) ns = 0
        | otherwise = let
            ds = zipWith (-) (tail ns) ns
            in head ns - next ds

main = (solve . parse) <$> getContents >>= print

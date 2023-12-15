import Data.List.Split(splitOn)

parse = splitOn "," . head . lines

hash :: String -> Int
hash = go 0 where
    go acc [] = acc
    go acc (x:xs) = go (((acc + fromEnum x) * 17) `mod` 256) xs

solve = sum . map hash

main = solve . parse <$> getContents >>= print

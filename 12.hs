import Data.List.Split(splitOn, wordsBy)

parse = map parseL . lines where
    parseL :: String -> (String, [Int])
    parseL l = let
        [code, counts] = words l
        in (code, map read $ splitOn "," counts)

solve = sum . map countWays where
    countWays (code, counts) = length . filter satisfiesCounts $ generate code where
        satisfiesCounts t = (== counts) . map length $ wordsBy (== '.') t
        generate "" = [""]
        generate (c:cs) = let
            ts = generate cs
            in case c of
                '?' -> map ('.':) ts ++ map ('#':) ts
                otherwise -> map (c:) ts

main = (solve . parse) <$> getContents >>= print

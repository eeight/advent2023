import Data.List.Split(splitOn)
import qualified Data.Array as A

data Action = Drop | Replace Int

parse = map p . splitOn "," . head . lines where
    p x = case splitOn "=" x of
        [n, i] -> (n, Replace $ read i)
        _ -> (take (length x - 1) x, Drop)

hash :: String -> Int
hash = go 0 where
    go acc [] = acc
    go acc (x:xs) = go (((acc + fromEnum x) * 17) `mod` 256) xs

run = go [] where
    go s [] = s
    go s ((n, Drop):xs) = go (filter ((/= n) . fst) s) xs
    go s ((n, Replace k):xs) = go (doReplace s n k) xs where
        doReplace :: [(String, Int)] -> String -> Int -> [(String, Int)]
        doReplace s n k
            | any ((== n) . fst) s = map (\(a, b) -> (a, if a == n then k else b)) s
            | otherwise = s ++ [(n, k)]

scoreBox = sum . zipWith (*) [1..] . map snd

solve actions = let
    boxes = A.assocs . fmap (run . reverse) $ A.accumArray (flip (:)) [] (0, 255) $ [(hash x, (x, y)) | (x, y) <- actions]
    in sum [(i + 1) * scoreBox b | (i, b) <- boxes]

main = solve . parse <$> getContents >>= print

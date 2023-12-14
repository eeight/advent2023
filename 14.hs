import Data.List.Split(splitOn)
import Data.List(intercalate, transpose, partition)
parse = lines

tilt = intercalate "#" . map pack . splitOn "#" where
    pack xs = let (os, ds) = partition (== 'O') xs in os ++ ds

score :: String -> Int
score = sum . map fst . filter ((== 'O') . snd) . zip [1..] . reverse

solve t = let
    t' = map tilt . transpose $ t
    in sum $ map score t'

main = solve . parse <$> getContents >>= putStrLn


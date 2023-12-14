import Data.List.Split(splitOn)
import Data.List(intercalate, transpose, partition, foldl')
import qualified Data.Set as S

parse = lines

tilt = intercalate "#" . map pack . splitOn "#" where
    pack xs = let (os, ds) = partition (== 'O') xs in os ++ ds

score :: String -> Int
score = sum . map fst . filter ((== 'O') . snd) . zip [1..] . reverse

applyCycle :: [String] -> [String]
applyCycle t = let
    northT = (transpose, transpose)
    westT = (id, id)
    southT = (map reverse . transpose, transpose . map reverse)
    eastT = (map reverse, map reverse)

    in foldl' (\t (f1, f2) -> f2 (map tilt $ f1 t)) t [northT, westT, southT, eastT]

fPow n f x = let
    findLoop s x
        | x `S.member` s = (S.size s, x)
        | otherwise = findLoop (S.insert x s) (f x)
    (n1, x') = findLoop S.empty x
    (n2, x'') = findLoop S.empty x'
    in (iterate f x') !! ((n - n1) `mod` n2)


solve t = let
    t' = fPow 1000000000 applyCycle t
    in sum . map score $ transpose t'

main = solve . parse <$> getContents >>= print

import Data.List.Split(splitOn)
import qualified Data.Map as M
import Data.Map((!))

parse i = let
    p:_:ns = lines i
    parseN k = let
        [n, opts] = splitOn " = " k
        [l, r] = splitOn ", " opts
        in (n, (tail l, take 3 r))

    in (p, map parseN ns)


solve (p, ns) = let
    nextM = M.fromList ns
    next n 'L' = fst $ nextM ! n
    next n 'R' = snd $ nextM ! n
    states = iterate (\(n, (p:ps)) -> (next n p, ps)) ("AAA", cycle p)
    in length $ takeWhile ((/= "ZZZ") . fst) states


main = (solve . parse) <$> getContents >>= print

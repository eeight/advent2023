import Data.List(foldl', tails)
import Data.List.Split(splitOn)
import Data.Maybe(catMaybes)
import qualified Data.Map as M

data Cond = G | L deriving (Eq, Show)

data Test = Test Int Cond Int | Always deriving (Show)

type Part = [Int]
type Rule = (Test, String)
type Wf = [Rule]

xmas 'x' = 0
xmas 'm' = 1
xmas 'a' = 2
xmas 's' = 3

parse :: String -> [(String, Wf)]
parse t = let
    [wfs, _] = splitOn [""] $ lines t
    parseWf :: String -> (String, Wf)
    parseWf l = let
        [name, rules] = splitOn "{" l
        rules' = splitOn "," $ take (length rules - 1) rules
        parseRule :: String -> Rule
        parseRule r = case splitOn ":" r of
            [cond, next] -> let
                idx = xmas $ head cond
                n = read $ drop 2 cond
                f = case cond !! 1 of
                    '>' -> G
                    '<' -> L
                in (Test idx f n, next)
            [next] -> (Always, next)
        in (name, map parseRule rules')
    in map parseWf wfs

flipTest (Test idx L v) = Test idx G (v - 1)
flipTest (Test idx G v) = Test idx L (v + 1)

type Subspace = [(Int, Int)]
allRange = (1, 4001)

allRanges, noRanges :: [Subspace]
allRanges = [replicate 4 allRange]
noRanges = []

heads = reverse . map reverse . tails . reverse

applyTest :: Test -> Subspace -> Maybe Subspace
applyTest Always rs = Just rs
applyTest (Test idx c v) rs = let
    (a, b) = rs !! idx
    (a', b') = case c of
        L -> (a, min b v)
        G -> (max a (v + 1), b)
    in if a' < b'
            then Just $ take idx rs ++ [(a', b')] ++ drop (idx + 1) rs
            else Nothing

applyTests :: [Test] -> Subspace -> Maybe Subspace
applyTests ts rs = foldl' (\rs t -> rs >>= (applyTest t)) (Just rs) ts

applyHistory :: [Test] -> [Subspace] -> [Subspace]
applyHistory ts ss = let
    n = length ts
    ts' = map flipTest (take (n - 1) ts) ++ [last ts]
    in catMaybes $ map (applyTests ts') ss

volume :: Subspace -> Integer
volume = product . map (fromIntegral . uncurry (flip (-)))

solve wfs = let
    m = M.fromList wfs
    memo :: M.Map String [Subspace]
    memo = M.fromList $ [("A", allRanges), ("R", noRanges)] ++ map (\(n, wf) -> (n, solveWf wf)) wfs where
        solveWf rules = let
            rs = zipWith applyHistory (tail $ heads $ map fst rules) (map ((memo M.!) . snd) rules)
            in concat rs

    in sum $ map volume $ memo M.! "in"

main = solve . parse <$> getContents >>= print

import Data.List.Split(splitOn)
import qualified Data.Map as M
import Debug.Trace

type Part = [Int]
type Rule = (Part -> Bool, String)
type Wf = [Rule]

xmas 'x' = 0
xmas 'm' = 1
xmas 'a' = 2
xmas 's' = 3

parse :: String -> ([(String, Wf)], [Part])
parse t = let
    [wfs, parts] = splitOn [""] $ lines t
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
                    '>' -> (>)
                    '<' -> (<)
                in (\i -> f (i !! idx) n, next)
            [next] -> (const True, next)
        in (name, map parseRule rules')
    parsePart p = let
        p' = take (length p - 2) $ tail p
        in map (read . tail . dropWhile (/= '=')) $ splitOn "," p'
    in (map parseWf wfs, map parsePart parts)

solve (wfs, parts) = let
    m = M.fromList wfs
    run part = go "in" where
        go "A" = True
        go "R" = False
        go n = let
            rs = m M.! n
            (_, n'):_ = dropWhile (\(t, _) -> not (t part)) rs
            in go $ n'
    ps = filter run parts
    in sum $ concat ps

main = solve . parse <$> getContents >>= print

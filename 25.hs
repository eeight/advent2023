import Data.Tuple(swap)
import Data.Maybe(fromJust)
import Data.List.Split(splitOn)
import Data.List(foldl')
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S

parse t = let
    p l = let
        [v, us] = splitOn ":" l
        us' = words us
        out = map (v,) us'
        in out ++ map swap out
    es = concatMap p $ lines t
    vs = M.keys . M.fromList $ es
    vind = M.fromList $ zip vs [0..]
    out = M.fromListWith (++) [(vind M.! u, [vind M.! v]) | (u, v) <- es]
    in A.listArray (0, length vs - 1) $ M.elems out

type F = S.Set (Int, Int)

solve outs = let
    bfs :: F -> Int -> Int -> Maybe [Int]
    bfs flow u v = let
        go :: S.Set Int -> M.Map Int Int -> [Int] -> Maybe (M.Map Int Int)
        go visited prev us = let
            nexts = filter (\(w, z) -> not (z `S.member` visited) && not ((w, z) `S.member` flow)) $ concatMap (\w -> map (w,) $ outs A.! w) us
            us' = map snd nexts
            visited' = foldl' (flip S.insert) visited us'
            prev' :: M.Map Int Int
            prev' = foldl' (\p (w, z) -> M.insert z w p) prev nexts
            in if v `elem` us'
                then Just prev'
                else if null us' then Nothing else go visited' prev' us'

        in case go S.empty M.empty [u] of
            Nothing -> Nothing
            Just prev -> Just $ u:(reverse $ (takeWhile (/= u) $ iterate (prev M.!) v))

    addFlow :: F -> Int -> Int -> Maybe F
    addFlow flow u v = case bfs flow u v of
        Nothing -> Nothing
        Just path -> Just $ foldl' (flip S.insert) flow (zip path (tail path))

    get3Flow :: Int -> Int -> Maybe F
    get3Flow u v = let
        f :: F
        f = iterate (\f -> fromJust $ addFlow f u v) S.empty !! 3
        in case addFlow f u v of
            Nothing -> Just f
            _ -> Nothing

    Just flow = head $ dropWhile ((== Nothing)) $ [get3Flow 0 v | v <- A.range $ A.bounds outs]

    dfs visited v
        | v `S.member` visited = visited
        | otherwise = foldl' dfs (S.insert v visited) $ filter (\u -> not $ S.member (v, u) flow) $ outs A.! v

    s1 = S.size $ dfs S.empty 0
    s2 = 1 + snd (A.bounds outs) - s1
    in s1*s2

main = solve . parse <$> getContents >>= print

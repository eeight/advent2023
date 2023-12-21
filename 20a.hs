{-# LANGUAGE OverloadedLists #-}

import Data.List.Split(splitOn)
import qualified Data.Map as M
import qualified Data.Array as A
import Data.List(nub, foldl', foldl1')
import Data.Array.ST(STArray)
import Data.STRef
import Control.Monad.ST
import Data.Array.MArray
import qualified Deque.Strict as D
import Control.Monad(liftM2, replicateM, when)

data Module = FF Bool | Conj [Bool] | Bcast | Sink deriving (Eq, Show)

parse = map p . lines where
    p l = let
        [n, dest] = splitOn " -> " l
        ds = filter (not . null) $ splitOn ", " dest
        (t, n') = case n of
            "broadcaster" -> (Bcast, n)
            "sink" -> (Sink, n)
            '&':x -> (Conj [], x)
            '%':x -> (FF False, x)
        in (t, n', ds)

solve es = let
    allNames = nub $ concatMap (\(_, n, os) -> n:os) es
    size = length allNames
    nameToN = M.fromList $ zip allNames [0..]
    nToName = M.fromList $ zip [0..] allNames
    vs = M.fromListWith const $ map (, Sink) allNames ++ map (\(t, n, _) -> (n, t)) es
    ins = M.fromListWith (++) $ map (, []) allNames ++ concatMap (\(_, n, os) -> map (,[n]) os) es
    outs = M.fromListWith const $ map (, []) allNames ++ map (\(_, n, os) -> (n, os)) es
    bCastN = nameToN M.! "broadcaster"

    rxInd = nameToN M.! "rk"

    makeInit (FF _, _) = FF False
    makeInit (Conj _, n) = Conj $ replicate (length $ ins M.! n) False
    makeInit (x, _) = x

    outN :: A.Array Int [(Int, Int)]
    outN = A.listArray (0, size - 1) $ map (\v -> map (\u -> (nameToN M.! u, length $ takeWhile (/= v) $ ins M.! u)) (outs M.! v)) allNames

    initStates = map (makeInit . (\n -> (vs M.! n, n))) allNames

    runSignal m@(FF _) _ True = (m, Nothing)
    runSignal (FF x) _ False = (FF (not x), Just $ not x)
    runSignal Bcast _ s = (Bcast, Just s)
    runSignal Sink _ _ = (Sink, Nothing)
    runSignal (Conj ss) n s = let
        ss' = take n ss ++ [s] ++ drop (n + 1) ss
        in (Conj ss', Just $ not $ all id ss')

    makeST :: [a] -> ST s (STArray s Int a)
    makeST l = newListArray (0, length l - 1) l

    loopLen beginInd rxInd = runST $ do
        state <- makeST initStates
        queue <- newSTRef $ []
        rxHit <- newSTRef False
        let pressB = do
                d <- readSTRef queue
                case D.uncons d of
                    Nothing -> return ()
                    Just ((i, j, s), d') -> do
                        when (i == rxInd && not s) $ writeSTRef rxHit True
                        vs <- readArray state i
                        let (vs', outS) = runSignal vs j s
                        let outs = (outN A.! i)
                        case outS of
                            Nothing -> writeSTRef queue d'
                            Just outS' -> do
                                writeSTRef queue $ foldl' (flip D.snoc) d' (map (\(x, y) -> (x, y, outS')) outs)
                        writeArray state i vs'
                        pressB

        let loop2 n = do
                        writeSTRef queue [(beginInd, 0, False)]
                        pressB
                        hit <- readSTRef rxHit
                        if hit then return n else loop2 (n + 1)
        loop2 1

    loopLen' (x, y) = loopLen (nameToN M.! x) (nameToN M.! y)
    ans :: Integer
    ans = foldl1' lcm $ map (fromIntegral . loopLen') [("vj", "rk"), ("gr", "zf"), ("xk", "cd"), ("fb", "qx")]
    in ans


main = (solve . parse) <$> getContents >>= print

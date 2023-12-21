{-# LANGUAGE OverloadedLists #-}

import Data.List.Split(splitOn)
import qualified Data.Map as M
import qualified Data.Array as A
import Data.List(nub, foldl')
import Data.Array.ST(STArray)
import Data.STRef
import Control.Monad.ST
import Data.Array.MArray
import qualified Deque.Strict as D
import Control.Monad(liftM2, replicateM)

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

    (lo, hi) = runST $ do
        state <- makeST initStates
        countLow <- newSTRef 0
        countHigh <- newSTRef 0
        queue <- newSTRef $ []
        let loop = do
                d <- readSTRef queue
                case D.uncons d of
                    Nothing -> return ()
                    Just ((i, j, s), d') -> do
                        vs <- readArray state i
                        let (vs', outS) = runSignal vs j s
                        let outs = (outN A.! i)
                        case outS of
                            Nothing -> writeSTRef queue d'
                            Just outS' -> do
                                modifySTRef' (if outS' then countHigh else countLow) (+ (length outs))
                                writeSTRef queue $ foldl' (flip D.snoc) d' (map (\(x, y) -> (x, y, outS')) outs)
                        writeArray state i vs'

                        loop

        replicateM 1000 $ do
            writeSTRef queue [(bCastN, 0, False)]
            modifySTRef' countLow (+1)
            loop
        liftM2 (,) (readSTRef countLow) (readSTRef countHigh)

    in (fromIntegral lo * fromIntegral hi) :: Integer

main = (solve . parse) <$> getContents >>= print

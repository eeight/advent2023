import qualified Data.Map as M
import Data.List(sort)

data Card = Card { uncard :: Char } deriving (Eq)

instance Ord Card where
    compare (Card x) (Card y) = let
        rank c = length $ dropWhile (/= c) "AKQT98765432J"
        in compare (rank x) (rank y)

data Strength = High | Pair | TwoP | Three | FullH | Four | Five deriving (Eq, Ord, Show)

parse :: String -> [([Card], Int)]
parse = map p . lines where
    p l = let
        [h, b] = words l
        in (map Card h, read b)

classify :: [Card] -> Strength
classify cs = let
    counts = M.fromListWith (+) $ map (,1) cs
    js = maybe 0 id $ M.lookup (Card 'J') counts
    cc = M.fromListWith (+) $ map ((,1) . snd) $ filter ((/= (Card 'J')) . fst) $ M.toList counts
    has n m = M.lookup (m - js) cc >= Just n
    has' n m = M.lookup m cc == Just n
    s
        | has 1 5 || js == 5 = Five
        | has 1 4 = Four
        | has' 1 3 && has' 1 2 = FullH
        | has' 2 2 && js == 1 = FullH
        | has 1 3 = Three
        | has' 2 2 = TwoP
        | has' 1 2 && js == 1 = TwoP
        | has 1 2 = Pair
        | otherwise = High
    in s

solve xs = let
    ys = zip [1..] $ sort $ map (\(cs, bet) -> ((classify cs, cs), bet)) xs
    in sum [fromIntegral $ bet * i | (i, (_, bet)) <- ys]

main = (solve . parse) <$> getContents >>= print

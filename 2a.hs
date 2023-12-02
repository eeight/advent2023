import Data.List.Split(splitOn)

data Colors = Colors Int Int Int
data Game = Game { gameId :: Int, gameTurns :: [Colors] }

power (Colors r g b) = r * g * b

gamePower = power . go (Colors 0 0 0) where
    go acc [] = acc
    go (Colors r g b) (Colors r' g' b':xs) = go (Colors (max r r') (max g g') (max b b')) xs

parse l = let
    [g, l'] = splitOn ": " l
    gId = read . last . words $ g
    parseTurn = go (Colors 0 0 0). splitOn ", " where
        go acc [] = acc
        go (Colors r g b) (x:xs) = let
            [n, c] = words x
            n' = read n
            acc' = case c of
                "red" -> Colors (r + n') g b
                "green" -> Colors r (g + n') b
                "blue" -> Colors r g (b + n')
            in go acc' xs

    turns = map parseTurn . splitOn "; " $ l'
    in Game gId turns

solve = sum . map (gamePower . gameTurns) . map parse . lines

main = solve <$> getContents >>= print

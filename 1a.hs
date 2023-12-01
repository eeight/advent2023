import Data.Char(isDigit)

patterns = [(pure . toEnum . (+ (fromEnum '0')) $ x, x) | x <- [1..9]] ++ [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)]

findDigit :: String -> [(String, Int)] -> [(String, Int)] -> Int
findDigit [] [] _ = undefined
findDigit (_:ss) [] pp = findDigit ss pp pp
findDigit s ((p', p''):pp) ppp
    | length s >= length p' && take (length p') s == p' = p''
    | otherwise = findDigit s pp ppp

decode :: String -> Int
decode s = let
    patterns' = [(reverse p', p'') | (p', p'') <- patterns]
    a = findDigit s patterns patterns
    b = findDigit (reverse s) patterns' patterns'
    in a*10 + b

main = (solve <$> getContents) >>= print where
    solve = sum . map decode . lines

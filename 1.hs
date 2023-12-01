import Data.Char(isDigit)

decode :: String -> Int
decode l = read $ (f l):(f . reverse $ l):[] where
    f = head . dropWhile (not . isDigit)

main = (solve <$> getContents) >>= print where
    solve = sum . map decode . lines

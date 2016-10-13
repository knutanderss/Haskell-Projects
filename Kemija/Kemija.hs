main = interact solve

vowel = "aeiou"

solve :: String -> String
solve [] = []
solve (x:xs) 
    | x `elem` vowel = x:solve ( drop 2 xs)
    | otherwise = x:solve xs

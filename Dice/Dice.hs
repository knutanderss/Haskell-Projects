main = do
    a <- getLine
    b <- getLine
    putStrLn $ solve a b

solve :: String -> String -> String
solve a b =
    let c = findValue (map read (words a))
        d = findValue (map read (words b))
    in if c > d then "Gunnar"
        else if c < d then "Emma"
        else "Tie"

findValue :: [Double] -> Double
findValue [a, b, c, d] = (a+b)/2 + (c+d)/2

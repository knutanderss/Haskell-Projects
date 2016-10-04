main = do
    n <- readInt
    putStrLn (createString "Abracadabra" 1 n)

readInt :: IO Int
readInt = do
    inp <- getLine
    return (read inp)

createString :: String -> Int -> Int -> String
createString s n top =
    if n==top
    then (show n) ++ " " ++ s
    else (show n) ++ " " ++ s ++ "\n" ++ (createString s (n+1) top)
    

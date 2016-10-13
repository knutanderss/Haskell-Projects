import Data.List

main = do
    a <- getLine
    b <- getLine
    c <- getLine
    let (x,y) = toList [a,b,c] [] []
    putStrLn $ solve x y

toList :: [String] -> [Int] -> [Int] -> ([Int], [Int])
toList [] a b = (a, b)
toList (x:xs) a b = 
    let w = map read (words x)
    in toList xs (w!!0:a) (w!!1:b)

solve :: [Int] -> [Int] -> String
solve xP yP =
    let x = getOc xP
        y = getOc yP
    in show (findValue x) ++ " " ++ show (findValue y)
    where getOc = map (\xs@(x:_) -> (x, length xs)) . group . sort

findValue :: [(Int, Int)] -> Int
findValue [] = error "What??"
findValue ((a,b):xs) = if b `mod` 2 /= 0 then a else findValue xs
    

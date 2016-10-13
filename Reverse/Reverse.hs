main = interact (unlines . solve . lines)

solve :: [String] -> [String]
solve [] = []
solve (('0':q):xs)= []
solve (s:sx) = reverse (map (rot i) word) : solve sx
    where   x:word:_ = words s
            i  = read x :: Int

rot :: Int -> Char -> Char
rot i c =
    let a = (w - fromEnum 'A' + i) `mod` 28
        b = if a<0 then a+28 else a
        c' | b==26 = '_' | b==27 = '.' | otherwise = toEnum (b + fromEnum 'A') :: Char
    in  c'
    where w | c == '_' = 26+65 | c == '.' = 27+65 | otherwise = fromEnum c

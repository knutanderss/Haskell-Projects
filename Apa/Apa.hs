main = interact solve
solve [] = []
solve (x:xs) = x : solve (dropWhile (==x) xs)

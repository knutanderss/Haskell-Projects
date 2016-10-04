pieces = [1,1,2,2,2,8]    

main = do
    list <- readIntegerList
    putStrLn (checkPieces pieces list)

checkPieces :: [Int] -> [Int] -> String
checkPieces [] _ = []
checkPieces (x:xs) (y:ys) = ((show (x-y)) ++ " "  ++ checkPieces xs ys)

readIntegerList :: IO [Int]
readIntegerList = do
    line <- getLine
    let numberList = splitOnChar line
    return (map read numberList)

splitOnChar :: String -> [String]
splitOnChar [] = []
splitOnChar s = 
    if ' ' `elem` s
    then ((takeWhile (/=' ') s) : splitOnChar (tail (dropWhile (/=' ') s)))
    else s:[]


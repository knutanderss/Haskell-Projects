import System.Random
import Data.Map
import Data.Ord
import Data.List
import Text.Read

main = do 
    sum <- throwDices 5 1 0 
    putStrLn $ "Poeng totalt: " ++ (show sum)

getMaxFromRound :: [Int] -> Int
getMaxFromRound xs =
    let points = toList $ fromListWith (+) [(x, 1) | x <- xs ]
        (x,n) = maximumBy (\(a,b) (c,d) -> compare (a*b) (c*d)) points
    in  (x*n)

throwDices :: Int -> Int -> Int -> IO Int
throwDices rounds curR curS 
    | curR <= rounds = do
        putStrLn $ "Runde " ++ (show curR)
        midlDiceList <- getDiceList 5 []
        printDices midlDiceList 1
        diceList <- rethrow midlDiceList 2
        let max = getMaxFromRound diceList
        putStrLn $ "Poeng: " ++ (show max)
        throwDices rounds (curR+1) (curS+max)
    | otherwise = return curS

rethrow :: [Int] -> Int -> IO [Int]
rethrow l 0 = return l
rethrow l n = do
    putStrLn $ "Omkast " ++ (show (3-n)) ++ ". Skriv terningnummer (1-5) for aa kaste om, eller 0 for beholde terningene slik de er:"
    s <- getNumber
    if s == 0 then return l 
    else do
        newThrow <- getStdRandom (randomR (1,6))
        let newList = take (s-1) l ++ [newThrow] ++ drop s l
        printDices newList 1
        rethrow newList (n-1)
    
printDices :: [Int] -> Int -> IO ()
printDices [] _ = return ()
printDices (x:xs) n = do
    putStrLn $ "Terning " ++ (show n) ++ ": " ++ (show x)
    printDices xs (n+1)
    
            
getNumber :: IO Int
getNumber = do
    l <- getLine
    let maybe = readMaybe l
    case maybe of
        Nothing -> error "Ikke et gyldig tall!"
        Just x -> return x
            
    
getDiceList :: Int -> [Int] -> IO [Int]
getDiceList 0 acc = return $ reverse acc
getDiceList n acc = do
    r <- getStdRandom (randomR (1,6))
    getDiceList (n-1) (r:acc)

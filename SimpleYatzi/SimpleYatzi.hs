import System.Random
import Data.Map
import Data.Ord
import Data.List


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
        diceList <- getDiceList 5 []
        let max = getMaxFromRound diceList
        putStrLn $ "Poeng: " ++ (show max)
        throwDices rounds (curR+1) (curS+max)
    | otherwise = return curS
    
getDiceList :: Int -> [Int] -> IO [Int]
getDiceList 0 acc = return $ reverse acc
getDiceList n acc = do
    r <- getStdRandom (randomR (1,6))
    putStrLn $ "Terning " ++ (show (6-n)) ++ ": " ++ (show r)
    getDiceList (n-1) (r:acc)

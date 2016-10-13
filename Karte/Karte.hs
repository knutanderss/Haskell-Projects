import Data.Maybe
import Data.Char
import Data.List

main = do
    line <- getLine
    putStrLn $ solve line

solve:: String -> String
solve (xs) 
    | isJust res = toStr (fromJust res)
    | otherwise = "GRESKA"  
    where 
        res = greska xs (13,13,13,13)

toStr :: (Int, Int, Int, Int) -> String
toStr (p,w,h,t) = (show p)++" "++(show w)++" "++(show h)++" "++ (show t)

greska :: String -> (Int, Int, Int, Int) -> Maybe (Int, Int, Int, Int)
greska [] (p,k,h,t) = Just (p,k,h,t)
greska (x:y:z:xs) (p,k,h,t)
    | (x:y:z:[]) `isInfixOf` xs = Nothing
    | x == 'P' = greska xs (p-1, k, h, t)
    | x == 'K' = greska xs (p, k-1, h, t)
    | x == 'H' = greska xs (p, k, h-1, t)
    | x == 'T' = greska xs (p, k, h, t-1)
    | otherwise = error "Fuckup"
greska xs _ = error xs

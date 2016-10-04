import Data.List.Split
import Data.Maybe
import Data.List

ascii = [
    "**** ** ** ****",
    "  *  *  *  *  *",
    "***  *****  ***",
    "***  ****  ****",
    "* ** ****  *  *",
    "****  ***  ****",
    "****  **** ****",
    "***  *  *  *  *",
    "**** ***** ****",
    "**** ****  ****"]

asciiToNumber :: Int -> String
asciiToNumber i = unlines (chunksOf 3 (ascii!!i))

solve :: [String] -> String
solve list@(x:xs) = 
    let n = ((length x) + 1) `div` 4
        digits = (asciiInputToStrings list (take n (repeat "")))
    in  checkModulus digits

checkModulus :: [String] -> String
checkModulus l
    | isNothing (parseDigits l) = "BOOM!!"
    | otherwise =
        let s = read (fromJust(parseDigits l))
        in  if s `mod` 6 == 0 then "BEER!!"
            else "BOOM!!"

-- Takes list digits (As ascii-strings) and converts to numbers % 6
parseDigits :: [String] -> Maybe String
parseDigits [] = Just []
parseDigits (x:xs)
    | isNothing (elemIndex x ascii) = Nothing
    | otherwise = 
        let next = parseDigits xs
        in  if isNothing next then Nothing
            else Just (show (fromJust (elemIndex x ascii)) ++ fromJust next)

-- Split a line into bunch of 3 and 3 chars (with spaces between)
getChunksOfThree :: String -> [String]
getChunksOfThree [] = []
getChunksOfThree l = take 3 l : getChunksOfThree (drop 4 l)

-- Takes input lines (and result) and converts to list of digits
asciiInputToStrings :: [String] -> [String] -> [String]
asciiInputToStrings [] result = result
asciiInputToStrings (x:xs) result = 
    let newResult = appendListElemToListElem (getChunksOfThree x) result
    in  asciiInputToStrings xs newResult 

-- Appends every elem in the first list to responding elem in second list
appendListElemToListElem :: [String] -> [String] -> [String]
appendListElemToListElem [] _ = []
appendListElemToListElem (x:xs) (y:ys) = (y++x) : (appendListElemToListElem xs ys)

main = do
    i <- sequence (take 5 (repeat getLine))
    putStrLn $ solve i

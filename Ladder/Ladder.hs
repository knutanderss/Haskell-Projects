degreeToRadian :: Double -> Double
degreeToRadian d = (d*pi)/180

findLadderHeight :: Double -> Double -> Integer
findLadderHeight h v =
    let d = h / (sin (degreeToRadian v))
    in ceiling d

solve :: [Integer] -> [Integer]
solve (x:y:[]) = [findLadderHeight (fromIntegral x) (fromIntegral y)]

readInput :: String -> [Integer]
readInput = (map read) . words
writeOutput :: [Integer] -> String
writeOutput = unlines . (map show)

main = do
    line <- getLine
    let s = solve (readInput line)
    putStrLn ( writeOutput s)
    



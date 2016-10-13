main = interact (unlines . map (solve.words) . lines)

solve :: [String] -> String
solve ["0","0","0"] = []
solve xs =
    let numbers = map read xs
        true = show (pi*(numbers!!0)^2)
        ex = show ((2*(numbers!!0))^2*(numbers!!2)/(numbers!!1))
    in  true ++ " " ++ ex


run :: String -> (String, [Int])
run s = 
    let (r, a, _,_) = eval s "" (repeat 0) 0
    in (r, (take 10 a))

eval :: String -> String -> [Int] -> Int -> (String, [Int], Int, String)
eval [] r a p = (r, a, p, [])
eval (x:xs) r a p
    | x == '+' =
        let a2 = append1ToArray p a (+)
        in  eval xs r a2 p
    | x == '-' =
        let a2 = append1ToArray p a (-)
        in eval xs r a2 p
    | x == '>' = eval xs r a (p+1)
    | x == '<' = eval xs r a (p-1)
    

append1ToArray :: Int -> [Int] -> (Int -> Int -> Int) -> [Int]
append1ToArray 0 (y:ys) o = ((o y 1):ys)
append1ToArray n (y:ys) o  = y : (append1ToArray (n-1) ys o)

checkIfZero :: [Int] -> Int -> Bool
checkIfZero (x:xs) 0 = x==0
checkIfZero (x:xs) n = checkIfZero xs (n-1)

import Data.List
import Text.Printf

main = do
    info <- getLine
    let [k,n] = map read (words info)
    employees <- sequence (take n (repeat getLine))
    putStrLn (solve k employees)

type Id = Int
type Salary = Int
type Prod = Int
type ProdSal = Double
type Recommended = Int
data Employee = Employee Id Salary Prod ProdSal Recommended deriving (Show, Eq)
instance Ord Employee where
    (Employee _ _ _ a _) `compare` (Employee _ _ _ b _) = a `compare` b


makeEmployee :: Int -> Int -> Int -> Int -> Employee
makeEmployee id sal prod rec = Employee id sal prod ((fromIntegral prod) / (fromIntegral sal)) rec

getEmployeeList :: [String] -> Int -> [Employee]
getEmployeeList [] _ = []
getEmployeeList (x:xs) n =
    let [sal,prod,rec] = map read (words x)
    in  makeEmployee n sal prod rec : getEmployeeList xs (n+1)

getBestEmployee :: [Employee] -> [Int] -> (Employee, [Employee], [Int])
getBestEmployee emps rec = 
    let best = minimum (filter (empIsApproved rec) emps)
    in  (best, (delete best emps), (rec ++ [getId best]))

pickEmployeesToTeam :: [Employee] -> [Int] -> Int -> [Employee]
pickEmployeesToTeam emps _ 0 = []
pickEmployeesToTeam emps recs n = 
    let (best, empsLeft, ids) = getBestEmployee emps recs
    in best : pickEmployeesToTeam empsLeft ids (n-1)

getId :: Employee -> Id
getId (Employee id _ _ _ _) = id

getSal :: Employee -> Salary
getSal (Employee _ sal _ _ _) = sal

getProd :: Employee -> Prod
getProd (Employee _ _ prod _ _) = prod

empIsApproved :: [Int] -> Employee -> Bool
empIsApproved [] _ = False
empIsApproved (x:xs) e@(Employee _ _ _ _ rec) =
    if x == rec then True else empIsApproved xs e

getTeamSum :: [Employee] -> Double
getTeamSum emps = (fromIntegral(sum(map getProd emps))) / fromIntegral(sum(map getSal emps))
    

solve :: Int -> [String] -> String
solve k employees = 
    let team = pickEmployeesToTeam (getEmployeeList employees 1) [0] k
    in  printf "%.3f" (getTeamSum team)



















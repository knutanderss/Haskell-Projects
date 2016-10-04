data Shape = Rect Int Int Int Int | Circle Int Int Int deriving (Show)
type Point = (Int, Int)

isInShape :: Shape -> Point -> Bool
isInShape (Rect x1 y1 x2 y2) (px, py) =
    if  px <= x2 && px >= x1 && py >= y1 && py <= y2 then True
    else False
isInShape (Circle x1 y1 r) (px, py) =
    if px >= x1-r && px <= x1+r && py >= y1 - r && py <= y1 + r then True
    else False

readInt :: IO Int
readInt = do
    l <- getLine
    return (read l)

makeDictOfShapes :: [[String]] -> [Shape]
makeDictOfShapes [] = []
makeDictOfShapes (("rectangle":x1:y1:x2:y2:[]):ys) =
    Rect (read x1) (read y1) (read x2) (read y2) : makeDictOfShapes ys
makeDictOfShapes (("circle":x:y:r:[]):ys) =
    Circle (read x) (read y) (read r) : makeDictOfShapes ys

makeDictOfPoints :: [[String]] -> [Point]
makeDictOfPoints [] = []
makeDictOfPoints ((x:y:[]):ys) = ((read x), (read y)) : makeDictOfPoints ys

solve :: [Shape] -> [Point] -> [String]
solve _ [] = []
solve shapes (p:ps) = show (length [ s | s<-shapes, isInShape s p]) : solve shapes ps

main = do
    shape <- readInt
    shapes <- sequence (take shape (repeat getLine))
    point <- readInt
    points <- sequence (take point (repeat getLine))
    let shapeDict = makeDictOfShapes $ map words shapes
        pointDict = makeDictOfPoints $ map words points
    putStr $ unlines (solve shapeDict pointDict)

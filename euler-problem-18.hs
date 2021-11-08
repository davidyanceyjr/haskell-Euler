 -- euler-problem-18.hs
triangleList :: [Int]
triangleList = [ 75, 95, 64, 17, 47, 82, 18, 35, 87, 10, 
                 20, 04, 82, 47, 65, 19, 01, 23, 75, 03, 
                 34, 88, 02, 77, 73, 07, 63, 67, 99, 65,
                 04, 28, 06, 16, 70, 92, 41, 41, 26, 56,
                 83, 40, 80, 70, 33, 41, 48, 72, 33, 47, 
                 32, 37, 16, 94, 29, 53, 71, 44, 65, 25, 
                 43, 91, 52, 97, 51, 14, 70, 11, 33, 28, 
                 77, 73, 17, 78, 39, 68, 17, 57, 91, 71, 
                 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 
                 48, 63, 66, 04, 68, 89, 53, 67, 30, 73,
                 16, 69, 87, 40, 31, 04, 62, 98, 27, 23, 
                 09, 70, 98, 73, 93, 38, 53, 60, 04, 23
               ]

 -- Check if given list is possible triangle.
--
possibleTriangles = take 500 $ scanl (+) 1 [2..]

isTriangle :: [Integer] -> Bool
isTriangle list = length list `elem` possibleTriangles

 -- Given a list which is a triangle calculate the sized of 
 -- the triangle.
--
sizeTriangle :: [a] -> Int
sizeTriangle [] = 0
sizeTriangle [x] = 0
sizeTriangle list = length $ takeWhile (<=length list) possibleTriangles
 
 -- Given a list(triangle) return a list of lists that
 -- represent the rows of the triangle. 
--
--  NOTE: the triangles rows are listed from bottom up.
-- 

parseTriangle :: Int -> [Int] -> [[Int]] -- Number of rows -> list -> triangle
parseTriangle rows list 
    | rows > 0 = drop allButLastRow list : parseTriangle (rows - 1) (take allButLastRow list)
    | otherwise = []
    where allButLastRow = length list - rows
 
 -- Given two lists (bottom two rows of a triangle) calculate 
 -- max sum path algorithm and return a new list for the bottom
 -- of the triangle.
--

calcLists :: [Int] -> [Int] -> [Int] --BottomRow -> TopRow -> NewBottom Row
calcLists [] _ = []
calcLists _ [] = []
calcLists (x:y:ys) (z:zs)
    | z + x > z + y = z + x : calcLists (y:ys) zs
    | otherwise = z + y : calcLists (y:ys) zs

-- Given a triangle perform the max sum path calculation
-- on the last two rows and create a new triangle performing
-- the calculation again recursively.
--

maxSumPath :: [[Int]] -> [[Int]]
maxSumPath [] = []
maxSumPath [x] = [x]
maxSumPath (x:y:xs) = maxSumPath (calcLists x y:xs)

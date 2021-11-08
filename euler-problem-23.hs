--Project Euler - problem 23
--
--A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
--For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

--A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

--As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24.
--By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.
--However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot 
--be expressed as the sum of two abundant numbers is less than this limit.

 -- ****************************************************************************************************************************
-- ****************************************************************************************************************************
--Every even integer greater than 46 can be written as the sum of 2 abundant numbers in at least 1 way.
--
--All multiples of an abundant number are also abundant.
--
--If P is a perfect number, then 2P, 3P, 4P… are all abundant numbers.
-- ****************************************************************************************************************************
-- ****************************************************************************************************************************
--
--Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

import System.IO

main :: IO ()
main = print $ sum abundantIntegers

 -- This section manufactures a list of all bundant numbers less than 28123.
-- ****************************************************************************************************************************
abundantIntegers = listAbundantNum 28123

listAbundantNum :: Int -> [Int]
listAbundantNum 0 = [0]
listAbundantNum maxAbundant = [y | y <- [1..maxAbundant], sum (faktors y) > y]

faktors :: Int -> [Int]
faktors 0 = [0]
faktors num = [x | x <- [1..num], num `mod` x == 0 && x < num]


-- ****************************************************************************************************************************
-- ****************************************************************************************************************************
 --  This Section filters all abundant numbers and their factors and mulitples.
 --
 ---- takes a list of Int [1..], a list of Abundunt Int and returns a list of non-sum abundant Int.
isSumOfTwoAbun :: [Int] -> [Int] -> [Int] 
isSumOfTwoAbun [] _ = []
isSumOfTwoAbun _ [] = []
isSumOfTwoAbun (x:xs) (y:ys)
    |x > y = isSumOfTwoAbun [1..50] ys
    |x < y && isAbundant (y-x) = isSumOfTwoAbun (x:xs) (y:ys)
    |otherwise = x : isSumOfTwoAbun xs (y:ys)

isAbundant :: Int -> Bool
isAbundant accu  
    | accu < sum (faktors accu) = True
    | otherwise = False


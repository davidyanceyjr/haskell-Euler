-- https://projecteuler.net/problem=16
-- euler-problem-16.hs  
--
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 2^1000?
--
-- Haskell library will make this excercise trivial....
-- We will use the Data.Char.digitToInt function.

import Data.Char

numString = show $ 2^1000
numList = map digitToInt numString
solution = sum numList

main = print solution

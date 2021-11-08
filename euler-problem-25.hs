 -- euler-problem-25.hs
 --
 --
--The Fibonacci sequence is defined by the recurrence relation:
--
--Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
--Hence the first 12 terms will be:
--
--F1 = 1
--F2 = 1
--F3 = 2
--F4 = 3
--F5 = 5
--F6 = 8
--F7 = 13
--F8 = 21
--F9 = 34
--F10 = 55
--F11 = 89
--F12 = 144
--The 12th term, F12, is the first term to contain three digits.
--
--What is the index of the first term in the Fibonacci sequence to contain 1000 digits? 
--
--The memoized version is much faster. Try memoized_fib 10000.

import Data.NumberLength

memoizedFib :: Int -> Integer
memoizedFib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoizedFib (n-2) + memoizedFib (n-1)
         
answer = head [x + 1 | x <- [1..], numberLength (memoizedFib x) < 1000] 

 --  euler-problem-21.hs
 --
 --Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 --If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
 
 --For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
 --The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 
 --Evaluate the sum of all the amicable numbers under 10000.
 --
 --
 --

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]                 
primeFactors n = factors n primes
  where factors n (p:ps) | p*p > n        = [n]
                         | n `mod` p == 0 = p : factors (n `div` p) (p:ps)
                         | otherwise      = factors n ps

primePowerFactors = map (\xs -> (head xs, length xs)) . group . primeFactors

sumProperDivisors n = product [((p^(a+1)) - 1) `div` (p - 1) | (p, a) <- primePowerFactors n] - n
  
amicablePairs n = cullPairs $ map generatePairs [2..n]
  where generatePairs m   = let s = sumProperDivisors m
                            in if m < s then (m, s) else (s, m)
        cullPairs []      = []
        cullPairs (x:xs)  = if x `elem` xs then x : cullPairs xs 
                            else cullPairs xs
                            
prob21 = sum $ map (\(x,y) -> x + y) $ amicablePairs 9999

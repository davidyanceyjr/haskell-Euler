-- https://projecteuler.net/problem=14
--
-- The following iterative sequence is defined for the set of positive integers.
--  n -> n/2 (n is even)
--  n -> 3n=1 (n is odd)
--
--  Using the rule above and starting with 13, we generate the following
--  sequence.
--
--  13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
--  It can be seen that this sequence (starting at 13 and finishing at 1)
--  contains 10 terms. Although it has not been proved yet (Collatz Problem), it
--  is thought that all starting numbers finish at 1.
--
--  Which starting number, under one million, produces the longest chain?
--

iterSeq :: Int -> [Int]
iterSeq 0 = []
iterSeq 1 = []
iterSeq n
    | odd n = compOdd : iterSeq compOdd
    | otherwise = compEven : iterSeq compEven
    where
        compEven = n `div` 2
        compOdd = 3 * n + 1

maxTerm :: Int -> Int -> Int -> Int -> Int
maxTerm n 0 cOfn p = cOfn
maxTerm n m cOfn p 
    | n > numTerms = maxTerm n (m - 1) cOfn (p - 1)
    | otherwise = maxTerm numTerms (m - 1) p (p - 1)
    where
        numTerms = length (iterSeq p)

main = print $ maxTerm 0 999999 0 999999

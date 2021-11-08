 --  euler-problem-20.hs
--  n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100! 
--
   
-- Convert Integer into list of numbers.
digs :: Integer -> [Integer]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

-- Solve: sum . digs $ foldl (*) 1 [1..100]
-- Answer: 648

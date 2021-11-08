-- Starting in the top left corner of a 2×2 grid, and only being able to move to
-- the right and down, there are exactly 6 routes to the bottom right corner.
--
-- How many such routes are there through a 20×20 grid?
--
import Data.Array

routes :: Int -> Int
routes size = arr ! (size, size)
  where
      arr = array ((0,0), (size,size)) [((x,y), inner (x,y) size) | x <-[0..size], y <-[0..size]]
      inner origin@(x,y) size 
          | x == 0 && y == 0 = 0
          | x == 0 || y == 0 = 1
          | otherwise = arr ! (x-1, y) + arr ! (x, y-1)

-- I didn't work this out on my own. My skill level is not quiet there yet. 
-- More study on memoization and laziness is required.

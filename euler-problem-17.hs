-- https://projecteuler.net/problem=17
-- euler-problem-17.hs
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- 
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
--
-- 
-- 
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
-- 
import Data.List

numOneToNine = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] -- will appear 90 times
numTenToTeens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] -- will appear ten times
numTensPlaces = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] -- will appear 9 times
hundred = ["hundred"] -- will appear 9 times
thousand = ["onethousand"] -- will appear once
theAnds = ["and"]

genFirstNineteen :: [String]
genFirstNineteen = numOneToNine ++ numTenToTeens

genTens :: [String] 
genTens = numTensPlaces ++ [x ++ y | y <- numOneToNine, x <- numTensPlaces]

genHundreds :: [String]
genHundreds = [x ++ "hundred" | x <- numOneToNine] ++ [y ++ "hundred" ++ "and" ++ x | y <- numOneToNine, x <- genFirstNineteen] ++ [z ++ y ++ "and" ++ x | y <- hundred, z <- numOneToNine, x <- genTens]

allWrittenNums = genFirstNineteen ++ genTens ++ genHundreds ++ thousand

--countNumOneToNine = 90 * length (concat numOneToNine)
--countNumTenToTeens = 10 * length (concat numTenToTeens)
--countNumTensPlaces = 9 * length (concat numTensPlaces)
--countHundred = 9 * length (concat hundred)
--countThousand = length (concat thousand)
--totalChars = countNumTenToTeens + countNumOneToNine + countHundred + countNumOneToNine + countNumTensPlaces + theAnds

--  euler-problem-22.hs
--
--Using euler-project-22_names.txt ..
--a 46K text file containing over five-thousand first names,
--begin by sorting it into alphabetical order.

--Then working out the alphabetical value for each name,
--multiply this value by its alphabetical position in the list to obtain a name score.

--For example, when the list is sorted into alphabetical order,
--COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
--So, COLIN would obtain a score of 938 × 53 = 49714.

--What is the total of all the name scores in the file?
--
import System.IO
import Control.Monad
import Data.List.Split
import Data.Maybe
import Data.List
import Data.Char

-- Begin by using splitOn to create a list from the contents of the file.
-- When read using readFile "contents" evaluates to a list with one long
-- element of strings.
--
-- Next, we sort the list created from contents. I clean the contents stripping
-- the non-alpha characters from the list. genScore uses the alphaValue to
-- generate a score from each word.
--
-- Finally, the totalScore is calculated using the product of the index with the
-- value. 
--
-- answer: 871198282

main = do  
        contents <- readFile "euler-project-22_names.txt"
        let listOfLists = map (sum . genScore . cleanContents) $ sort $ splitOn "," contents 
        print . sum $ totalScore 1 listOfLists

alphaValue = map toUpper ['0','a','b','c','d','e','f','g','h','i','j','k','l','m',
                          'n','o','p','q','r','s','t','u','v','w','x','y','z']

genScore :: String -> [Int]
genScore [] = []
genScore xs = map (fromJust . (`elemIndex` alphaValue)) xs

totalScore :: Int -> [Int] -> [Int]
totalScore _ []     = []
totalScore n (x:xs) = n * x : totalScore (n+1) xs

cleanContents :: String -> String
cleanContents [] = []
cleanContents xs   = filter isAlpha xs


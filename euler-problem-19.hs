 -- euler-problem-19.hs
--
 -- You are given the following information, but you may prefer to do some research for yourself.
--
 -- 1 Jan 1900 was a Monday.
--
 -- Thirty days has September,
 -- April, June and November.
-- 
 -- All the rest have thirty-one,
 -- Saving February alone,
--
 -- Which has twenty-eight, rain or shine.
 -- And on leap years, twenty-nine.
--
 -- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
 -- 
 -- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1900 to 31 Dec 2000)?
--
--
 -- The start date should be entered and the solution provided from that date. 
 -- In this Case 1 Jan 1900 to 31 Dec 2000.
--  **************************************************************************************
--  **************************************************************************************
--
--

monthsNonLeap   = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
monthsLeap      = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
data DaysOfWeek = Mon|Tue|Wed|Thu|Fri|Sat|Sun deriving (Show, Eq, Ord, Enum, Bounded)

startYear = 1900
endYear   = 2000

isLeapYear :: Int -> Bool
isLeapYear 0 = error "No year provided"
isLeapYear x
    | x `mod` 4         == 0 = True
    | x `mod` 400       == 0 = True
    | otherwise = False


countingSundays :: Int -> Int -> DaysOfWeek -> Int --Start year, End year, Day of Week
countingSundays 0 _ _  = undefined
countingSundays _ 0 _ = undefined

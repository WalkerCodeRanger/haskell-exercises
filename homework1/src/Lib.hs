module Lib where

import Data.Function

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
      | n > 0 = map (read . (:[])) (show n)
      | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2

-- double every other integer in a list, *starting from the back* i.e. the second
-- from the last, the fourth from the last, etc.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse xs
    & (zipWith (*) (cycle [1, 2]))
    & reverse

-- Exercise 3

-- Sum the digits of the integers in a list so [16, 2] = 1 + 6 + 2 = 9
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- Exercise 4

-- Validate that an integer could be a valid credit card number
validate :: Integer -> Bool
validate n
  | n > 0 = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0
  | otherwise = False

-- Exercise 5

-- These two types provided as part of assignment
type Peg = String
type Move = (Peg, Peg)

-- Returns a list of moves to move the stated number of disks from the first peg
-- to the second peg using the third peg as temp space
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disks from to temp
  | disks == 0 = []
  | otherwise = (hanoi (disks-1) from temp to)
    ++ (from, to) : (hanoi (disks-1) temp to from)

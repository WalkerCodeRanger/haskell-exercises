module Lib where

import Data.Function

toDigits :: Integer -> [Integer]
toDigits n
      | n > 0 = map (read . (:[])) (show n)
      | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- double every other integer in a list, *starting from the back* i.e. the second
-- from the last, the fourth from the last, etc.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse l
    & (zipWith (*) (cycle [1, 2]))
    & reverse

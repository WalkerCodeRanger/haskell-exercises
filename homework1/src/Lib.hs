module Lib where

toDigits :: Integer -> [Integer]
toDigits n
      | n > 0 = map (read . (:[])) (show n)
      | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

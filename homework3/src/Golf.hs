module Golf where

import Data.List

-- Note: The stated goal of this is to get the actual code as short as possible,
-- not counting imports, comments, whitespace and type declarations
--
-- **Because of this, the names are shorter than I would like**

-- Exercise 1

-- returns all skips of a list, i.e. the first result is the original list, the
-- second is every other item in the list, the third is every third item etc.
-- The result will be a number of lists equal to the length of the original list
skips :: [a] -> [[a]]
skips l = skip l <$> [0..length l - 1]

-- Skips items from a list, taking the items in between. i.e. skip 1 will give
-- every other item.
skip :: [a] -> Int -> [a]
skip l n = case drop n l of
  [] -> []
  x:r -> x:(skip r n)

-- Exercise 2

-- Find all elements that are greater than the element before and after them.
-- Note first and last can never be local maxima.
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:r) = let l = localMaxima $ b:c:r in
  if a < b && b > c then b:l else l
localMaxima _ = []


-- Exercise 3

-- Generate a histogram of the integers in a list. We are allowed to assume they
-- are all in the range 0..9 (values outside the range are ignored)
histogram :: [Integer] -> String
histogram v = unlines . transpose . padBars $ (bar v) <$> [0..9]

-- Generate a string that represents the bar for an individual value. Note that
-- due to the way transpose works, the bar starts with the asterisks and ends
-- with the value (i.e. "****=2").
bar :: [Integer] -> Integer -> [Char]
bar v n = replicate (count n v) '*' ++ "=" ++ show n

-- Pad out the bar strings so they are all the same length
padBars :: (Functor f, Foldable f) => f [Char] -> f [Char]
padBars b = pad (maximum (length <$> b)) <$> b

-- Count the number of occurrences of a value in a list
count :: Integer -> [Integer] -> Int
count n = length . filter (==n)

-- Left pads a string with spaces out to a given length
pad :: Int -> [Char] -> [Char]
pad n s = replicate (n - length s) ' ' ++ s

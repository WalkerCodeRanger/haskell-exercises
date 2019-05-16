module Golf where

-- Note: The stated goal of this is to get the actual code as short as possible,
-- not counting imports, comments, whitespace and type declarations

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
localMaxima (a:b:c:r)
  | a < b && b > c = b:(localMaxima $ b:c:r)
  | otherwise = (localMaxima $ b:c:r)
localMaxima _ = []

module Golf where

-- Exercise 1

-- returns all skips of a list, i.e. the first result is the original list, the
-- second is every other item in the list, the third is every third item etc.
-- The result will be a number of lists equal to the length of the original list
skips :: [a] -> [[a]]
skips xs = takeWhile (not . null) $ map (skip xs) [0..]

-- Skips items from a list, taking the items in between. i.e. skip 1 will give
-- every other item.
skip :: [a] -> Int -> [a]
skip l n = case drop n l of
  [] -> []
  x:xs -> x:(skip xs n)

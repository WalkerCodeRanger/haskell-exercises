module Golf where

-- Exercise 1

-- returns all skips of a list, i.e. the first result is the original list, the
-- second is every other item in the list, the third is every third item etc.
-- The result will be a number of lists equal to the length of the original list
skips :: [a] -> [[a]]
skips xs = takeWhile (not . null) $ map (flip skip xs) [0..]

-- Skips items from a list, taking the items in between. i.e. skip 1 will give
-- every other item.
skip :: Int -> [a] -> [a]
skip n l = case drop n l of
  [] -> []
  x:xs -> x:(skip n xs)

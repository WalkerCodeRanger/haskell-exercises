module Lib where

import Data.List

-- Exercise 1

-- fun1 function provided as part of assignment (fun1' is what I wrote)
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

-- fun2 function provided as part of assignment (fun2' is what I wrote)
-- This problem is based on the Collatz conjecture, however it isn't valid for
-- negative numbers. I've added a case to make that not run forever.
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | n < 1 = 0
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate next
  where next n
          | even n = (n `div` 2)
          | otherwise = (3 * n + 1)

-- Exercise 2
-- Generate a balanced binary tree using foldr

-- Tree type provided as part of assignment
data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node _ left r right) =
  let (newLeft, newRight) = if treeHeight left <= treeHeight right
                            then (treeInsert x left, right)
                            else (left, treeInsert x right) in
    let height = 1 + max (treeHeight newLeft) (treeHeight newRight) in
      Node height newLeft r newRight

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

treeBalanced :: Tree a -> Bool
treeBalanced Leaf = True
treeBalanced (Node _ left _ right) =
  abs (treeHeight left - treeHeight right) <= 1
    && (treeBalanced left)
    && (treeBalanced right)

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr xor' False
  where xor' a b = (a || b) && not (a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> f x : l) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = primes
  where
    remove = filter (<=n)
              . map (\(i, j) -> i+j+2*i*j)
              . filter (\(i, j) -> i <= j)
              $ cartProd [1..n] [1..n]
    remaining = [1..n] \\ remove
    primes = map (\x -> 2*x+1) remaining

-- cartProd function provided as part of assignment
-- The cartesian product of two lists
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

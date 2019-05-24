{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Exercise 1

-- Define Fibonacci as a basic recursive function
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Define the Fibonacci sequence in terms of the `fib` function (yes, this will be slow)
fibs1 :: [Integer]
fibs1 = fib <$> [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : fibs 0 1
  where fibs x y = let z = x + y in z : fibs y z

-- Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- the "ruler function" 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
-- where the nth element in the stream (assuming the first element corresponds
-- to n = 1) is the largest power of 2 which evenly divides n.
ruler :: Stream Integer
ruler = startRuler 0

interleaveStreams :: Stream a -> Stream a -> Stream a
-- This is the version I wrote first which was causing stack overflow because it
-- forced the evaluation of the second argument which was causing infinitive
-- recursion on `startRuler x+1`.
--interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))
-- I found this version in another persons solution. It makes sense when I read
-- it, but the other form seemed much more natural to me.
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- Even with the hint, had to look up how to form this sequence using interleaving
-- Eric D.Burgess - http://oeis.org/A001511
startRuler :: Integer -> Stream Integer
startRuler x = interleaveStreams (streamRepeat x) (startRuler (x+1))

-- Exercise 6

-- This gives a bunch of warnings, but I'm not going to go back and rename
-- everything just because the homework has a stupid unusual variable name
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0 + b0) (a' + b')
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (streamMap (*a0) b' + a' * b)

instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - a/b*b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix = Matrix Integer Integer
                     Integer Integer

instance Num Matrix where
  (*) (Matrix a11 a12
              a21 a22)
      (Matrix b11 b12
              b21 b22)
    = Matrix (a11*b11+a12*b21) (a11*b12+a12*b22)
             (a21*b11+a22*b21) (a21*b12+a22*b22)

fibMatrix :: Matrix
fibMatrix = Matrix 1 1
                   1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fibOf (fibMatrix^n)
  where fibOf (Matrix _ x _ _) = x


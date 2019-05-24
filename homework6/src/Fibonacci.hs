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

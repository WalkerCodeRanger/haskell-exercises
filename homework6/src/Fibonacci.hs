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

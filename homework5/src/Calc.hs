module Calc where

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

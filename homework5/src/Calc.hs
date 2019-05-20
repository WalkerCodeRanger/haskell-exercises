{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}

module Calc where

import Data.Coerce
import ExprT
import Parser
import qualified StackVM as VM

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4

instance Expr Integer where
  lit i = i
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit b = b > 0
  add x y = x || y
  mul x y = x && y

-- These two new types provided with question:
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit i = MinMax i
  add x y = MinMax $ max (coerce x) (coerce y)
  mul x y = MinMax $ min (coerce x) (coerce y)

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7
  add x y = Mod7 $ ((coerce x) + (coerce y)) `mod` 7
  mul x y = Mod7 $ ((coerce x) * (coerce y)) `mod` 7

-- Exercise 5
-- The directions for this exercise are a little confusing. Specifically, the
-- part "For any arithmetic expression `exp :: Expr a => a` it should be the
-- case that `stackVM exp == Right [IVal exp]`". This appears to imply that it
-- should be possible to compile different instances of the type class Expr
-- which wouldn't be possible. Also, the expression `Right [IVal exp]` doesn't
-- seem to be well typed unless `exp :: Integer`. I'm interpreting this to mean
-- for any arithmetic expression string the VM would produce the same result as
-- the integer evaluator.

instance Expr VM.Program where
  lit i = [VM.PushI i]
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

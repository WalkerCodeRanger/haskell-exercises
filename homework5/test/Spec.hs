import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import ExprT
import Calc

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20" $
    20 @=? eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

  , testCase "evalStr \"(2+3)*4\" == Just 20" $
    Just 20 @=? evalStr "(2+3)*4"
  , testCase "evalStr \"2+3*4\" == Just 14" $
    Just 14 @=? evalStr "2+3*4"
  , testCase "evalStr \"2+3*\" == Nothing" $
    Nothing @=? evalStr "2+3*"
  , testCase "mul (add (lit 2) (lit 3)) (lit 4) :: ExprT == Mul (Add (Lit 2) (Lit 3)) (Lit 4)" $
    Mul (Add (Lit 2) (Lit 3)) (Lit 4) @=? ((mul (add (lit 2) (lit 3)) (lit 4)) :: ExprT)
  ]

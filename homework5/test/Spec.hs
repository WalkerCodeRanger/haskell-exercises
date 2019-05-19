import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Parser
import ExprT
import Calc

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testM7 = testExp :: Maybe Mod7

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

  , testCase "testInteger" $
    Just (-7) @=? testInteger
  , testCase "testBool" $
    Just True @=? testBool
  , testCase "testMM" $
    Just (MinMax 5) @=? testMM
  , testCase "testM7" $
    Just (Mod7 0) @=? testM7
  ]

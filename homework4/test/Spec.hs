import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Lib
import Data.List as List

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "fun1' == fun1" $
      \list -> fun1 list == fun1' list
  , QC.testProperty "fun2' == fun2" $
      \x -> fun2 x == fun2' x
  , QC.testProperty "treeBalanced . foldTree" $
      \list -> treeBalanced $ foldTree (list::[Int])
  , QC.testProperty "map' == map" $
    \list -> map' (* (2::Int)) list == map (* 2) list
  ]

unitTests = testGroup "Unit tests"
  [ testCase "xor [False, True, False] == True" $
    True @=? xor [False, True, False]

  , testCase "xor [False, True, False, False, True] == False" $
    False @=? xor [False, True, False, False, True]
  ]

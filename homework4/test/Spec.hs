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
  , testCase "sieveSundaram 100" $
    [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
    73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151,
    157, 163, 167, 173, 179, 181, 191, 193, 197, 199] @=? sieveSundaram 100
  ]

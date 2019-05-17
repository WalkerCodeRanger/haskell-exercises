import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Lib

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps]


qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "fun1 == fun1'" $
      \list -> fun1 list == fun1' list
  , QC.testProperty "fun2 == fun2'" $
      \x -> fun2 x == fun2' x
  , QC.testProperty "treeBalanced . foldTree" $
      \list -> treeBalanced $ foldTree (list::[Int])
  ]

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit

import Data.List
import Lib

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps]


qcProps = testGroup "(checked by QuickCheck)"
  [{-- QC.testProperty "fun1 == fun1'" $
      \list -> fun1 list == fun1' list
  , QC.testProperty "fun2 == fun2'" $
      \x -> fun2 x == fun2' x
  ,--} QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Integer]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Fibonacci

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "take 15 fibs1" $
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
      @=? take 15 fibs1
  , testCase "take 15 fibs2" $
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
      @=? take 15 fibs1
  , testCase "streamRepeat 5" $
    take 100 (repeat 5) @=? take 100 (streamToList $ streamRepeat 5)
  , testCase "streamMap" $
    take 100 (repeat 10) @=? take 100 (streamToList $ streamMap (*2) (streamRepeat 5))
  , testCase "streamFromSeed" $
    take 100 (iterate (*2) 1) @=? take 100 (streamToList $ streamFromSeed (*2) 1)
  , testCase "nats" $
    take 100 [0..] @=? take 100 (streamToList nats)
  , testCase "ruler" $
    [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4] @=? take 16 (streamToList ruler)
  ]

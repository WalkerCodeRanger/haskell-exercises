import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import JoinList
import Sized
import Scrabble

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

-- Functions to help build a sample join list
val :: a -> JoinList Size a
val v = Single 1 v

sampleJL :: JoinList Size Char
sampleJL = (val 'y' +++ (val 'e' +++ val 'a')) +++ val 'h'

-- Safe list indexing function provided with the homework
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

unitTests = testGroup "Unit tests"
  [ testCase "+++" $
    ['y', 'e', 'a', 'h'] @=? jlToList sampleJL
  , testCase "indexJ" $
    (all (==True) [(jlToList sampleJL !!? i) == (indexJ i sampleJL) | i <- [-1..5]])
      @? "not equivalent to safe list indexing"
  , testCase "dropJ" $
    (all (==True) [jlToList (dropJ n sampleJL) == drop n (jlToList sampleJL) | n <- [-1..5]])
      @? "not equivalent to list drop"
  , testCase "takeJ" $
    (all (==True) [jlToList (takeJ n sampleJL) == take n (jlToList sampleJL) | n <- [-1..5]])
      @? "not equivalent to list take"
  , testCase "scoreLine" $
    scoreLine "yay " +++ scoreLine "haskell!" @?=
      Append (Score 23)
        (Single (Score 9) "yay ")
        (Single (Score 14) "haskell!")

  ]

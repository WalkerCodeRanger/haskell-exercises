import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import JoinList
import Sized

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

-- Join list to list function provided with the question
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

unitTests = testGroup "Unit tests"
  [ testCase "+++" $
    ['y', 'e', 'a', 'h'] @=? jlToList sampleJL
  , testCase "indexJ" $
    (all (==True) [(jlToList sampleJL !!? i) == (indexJ i sampleJL) | i <- [-1..5]])
      @? "not equivalent to safe list indexing"
  , testCase "dropJ" $
    (all (==True) [jlToList (dropJ n sampleJL) == drop n (jlToList sampleJL) | n <- [-1..5]])
      @? "not equivalent to list drop"
  ]

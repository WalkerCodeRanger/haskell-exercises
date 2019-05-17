import Test.HUnit
import Golf

skipsCases = [
    ("ABCD", ["ABCD", "BD", "C", "D"]),
    ("hello!", ["hello!", "el!", "l!", "l", "o", "!"])--,
    --([1], [[1]]),
    --([True,False], [[True,False], [False]]),
    --([], [])
  ]

testSkipsCase list expected = TestCase $ assertEqual ("for (skips "++(show list)++"),") expected (skips list)

testSkips = TestLabel "skips" $ TestList (map (uncurry testSkipsCase) skipsCases)

localMaximaCases = [
    ([2,9,5,6,1], [9,6]),
    ([2,3,4,1,5], [4]),
    ([1,2,3,4,5], [])
  ]

testLocalMaximaCase list expected = TestCase $ assertEqual ("for (localMaxima "++(show list)++"),") expected (localMaxima list)

testLocalMaxima = TestLabel "localMaxima" $ TestList (map (uncurry testLocalMaximaCase) localMaximaCases)

main :: IO Counts
main = runTestTT $ TestList [testSkips, testLocalMaxima]

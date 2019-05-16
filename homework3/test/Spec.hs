import Test.HUnit
import Golf

skipsCases :: [(String, [String])]
skipsCases = [
    ("ABCD", ["ABCD", "BD", "C", "D"]),
    ("hello!", ["hello!", "el!", "l!", "l", "o", "!"])--,
    --([1], [[1]]),
    --([True,False], [[True,False], [False]]),
    --([], [])
  ]

testSkipsCase :: (Eq a, Show a) => [a] -> [[a]] -> Test
testSkipsCase list expected = TestCase $ assertEqual ("for (skips "++(show list)++"),") expected (skips list)

testSkips :: Test
testSkips = TestLabel "skips" $ TestList (map (uncurry testSkipsCase) skipsCases)

localMaximaCases :: [([Integer], [Integer])]
localMaximaCases = [
    ([2,9,5,6,1], [9,6]),
    ([2,3,4,1,5], [4]),
    ([1,2,3,4,5], [])
  ]

testLocalMaximaCase :: [Integer] -> [Integer] -> Test
testLocalMaximaCase list expected = TestCase $ assertEqual ("for (localMaxima "++(show list)++"),") expected (localMaxima list)

testLocalMaxima :: Test
testLocalMaxima = TestLabel "localMaxima" $ TestList (map (uncurry testLocalMaximaCase) localMaximaCases)

main :: IO Counts
main = runTestTT $ TestList [testSkips, testLocalMaxima]

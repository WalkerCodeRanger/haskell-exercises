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

main :: IO Counts
main = runTestTT $ TestList [testSkips]

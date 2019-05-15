import Test.HUnit
import Lib

toDigitsCases = [
    (45626479, [4,5,6,2,6,4,7,9]),
    (1234, [1,2,3,4]),
    (0, []),
    (-17, [])
  ]

testToDigitsCase n expected = TestCase $ assertEqual ("for (toDigits "++(show n)++"),") expected (toDigits n)
testToDigits = TestLabel "toDigits" $ TestList (map (uncurry testToDigitsCase) toDigitsCases)

toDigitsRevCases = [
    (1234, [4,3,2,1]),
    (0, []),
    (-17, [])
  ]

testToDigitsRevCase n expected = TestCase $ assertEqual ("for (toDigitsRev "++(show n)++"),") expected (toDigitsRev n)
testToDigitsRev = TestLabel "toDigitsRev" $ TestList (map (uncurry testToDigitsRevCase) toDigitsRevCases)

doubleEveryOtherCases = [
    ([8,7,6,5], [16,7,12,5]),
    ([1,2,3], [1,4,3])
  ]

testDoubleEveryOtherCase n expected = TestCase $ assertEqual ("for (doubleEveryOther "++(show n)++"),") expected (doubleEveryOther n)
testDoubleEveryOther = TestLabel "doubleEveryOther" $ TestList (map (uncurry testDoubleEveryOtherCase) doubleEveryOtherCases)

main :: IO Counts
main = runTestTT $ TestList [testToDigits, testToDigitsRev, testDoubleEveryOther]

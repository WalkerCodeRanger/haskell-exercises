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

testDoubleEveryOtherCase xs expected = TestCase $ assertEqual ("for (doubleEveryOther "++(show xs)++"),") expected (doubleEveryOther xs)
testDoubleEveryOther = TestLabel "doubleEveryOther" $ TestList (map (uncurry testDoubleEveryOtherCase) doubleEveryOtherCases)

sumDigitsCases = [
    ([16,7,12,5], 22),
    ([16,2], 9)
  ]

testSumDigitsCase xs expected = TestCase $ assertEqual ("for (sumDigits "++(show xs)++"),") expected (sumDigits xs)
testSumDigits = TestLabel "sumDigits" $ TestList (map (uncurry testSumDigitsCase) sumDigitsCases)

validateCases = [
    (4012888888881881, True),
    (4012888888881882, False),
    (-4012888888881881, False),
    (0, False)
  ]

testValidateCase n expected = TestCase $ assertEqual ("for (validate "++(show n)++"),") expected (validate n)
testValidate = TestLabel "validate" $ TestList (map (uncurry testValidateCase) validateCases)


main :: IO Counts
main = runTestTT $ TestList [testToDigits,
    testToDigitsRev,
    testDoubleEveryOther,
    testSumDigits,
    testValidate]

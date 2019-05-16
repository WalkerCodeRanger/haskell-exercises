import Test.HUnit
import Log
import LogAnalysis
import Lib

parseMessageCases = [
    ("E 2 562 help help", LogMessage (Error 2) 562 "help help"),
    ("I 29 la la la", LogMessage Info 29 "la la la"),
    ("W 42 A warning", LogMessage Warning 42 "A warning"),
    ("This is not in the right format", Unknown "This is not in the right format")
  ]

testParseMessageCase line expected = TestCase $ assertEqual ("for (parseMessage "++(show line)++"),") expected (parseMessage line)
testParseMessage = TestLabel "parseMessage" $ TestList (map (uncurry testParseMessageCase) parseMessageCases)

main :: IO Counts
main = runTestTT $ TestList [testParseMessage]

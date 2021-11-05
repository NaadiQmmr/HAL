
import Test.HUnit

import Lib
import CombinedParsers
import ParserLib

main :: IO ()
main = runTestTT testCleanSpaces >>
     runTestTT testRunParser >>
     putStrLn "Tests done"

testCleanSpaces :: Test
testCleanSpaces = TestCase $ assertEqual
        "Should remove spaces" "HelloWorld" $ cleanSpaces "   \r\nHello World     "

testParserChar = TestCase $ assertEqual "Should get first character"
                (runParser char "Sheesh") (Just 'S')
testParserDigit = TestCase $ assertEqual "Get a digit if any"
                (runParser digit "0213q") (Just '0')
testParserHas = TestCase $ assertEqual "Should get first character if predicate"
                (runParser (has 'c') "char") (Just 'c')
testParserHas' = TestCase $ assertEqual "Should not get first character if pred"
                (runParser (has 'd') "char") Nothing

testRunParser :: Test
testRunParser = TestList [
    "parsing a character"           ~: testParserChar,
    "parsing a digit"               ~: testParserDigit,
    "parsing some character"        ~: testParserHas,
    "not parsing some character"    ~: testParserHas'
    ]

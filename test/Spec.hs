
import Test.HUnit

import Lib
import CombinedParsers
import ParserLib

main :: IO ()
main = runTestTT testCleanSpaces >> putStrLn "Tests done"

testCleanSpaces :: Test
testCleanSpaces = TestCase $ assertEqual
        "Should remove spaces" "HelloWorld" $ cleanSpaces "   \r\nHello World     "


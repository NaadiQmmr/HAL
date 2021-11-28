import Test.HUnit

import Lib
import CombinedParsers
import ParserLib
import Eval
import Tokens
import Repl

import TestLib
import TestEvalInvalidCases

--i love u

main :: IO ()
main = runTestTT testCleanSpaces >>
     runTestTT testRunParser >>
     putStrLn "Tests done"

testCleanSpaces :: Test
testCleanSpaces = TestCase $ assertEqual
        "Should remove spaces" "HelloWorld" $ cleanSpaces "   \r\nHello World     "

testParserChar = TestCase $ assertEqual "Should get first character"
                (Just 'S') (runParser char "Sheesh")
testParserDigit = TestCase $ assertEqual "Get a digit if any"
                (Just '0') (runParser digit "0213q")
testParserHas = TestCase $ assertEqual "Should get first character if predicate"
                (Just 'c') (runParser (has 'c') "char")
testParserHas' = TestCase $ assertEqual "Should not get first character if pred"
                Nothing (runParser (has 'd') "char")


testEvalString = TestCase (do
                                a <- runSingle "\"blabla\""
                                let b = "blabla"
                                assertEqual "Just return the string" b a)

testEvalBoolTrue = TestCase (do
                                a <- runSingle "#t"
                                let b = "#t"
                                assertEqual "Just return the boolean True" b a)

testEvalBoolFalse =  TestCase (do
                                a <- runSingle "#f"
                                let b = "#f"
                                assertEqual "Just return the boolean False" b a)

testEvalNumber = TestCase (do
                                a <- runSingle "0"
                                let b = "0"
                                assertEqual "Just return the number" b a)

testEvalNegNumber =  TestCase (do
                                a <- runSingle "(- 3)"
                                let b = "-3"
                                assertEqual "Just return the negative number" b a)
 
testEvalAtom = TestCase (do
                                a <- runSingle "'word"
                                let b = "word"
                                assertEqual "Just return the atom" b a)

testListCompo1 = TestCase (do
                                a <- runSingle "'(1)"
                                let b = "(1)"
                                assertEqual "Basic quote" b a) 

testListCompo2 = TestCase (do
                                a <- runSingle "(- 4 1)"
                                let b = "3"
                                assertEqual "Should compute the result" b a)

testListCompo3 = TestCase (do
                                a <- runSingle "(cons 4 '())"
                                let b = "(4)"
                                assertEqual "Should construct a proper List" b a)

testListCompo4 =  TestCase (do
                                a <- runSingle "(cons 1 (cons 2 '()))"
                                let b = "(1 2)"
                                assertEqual "Should construct a proper List" b a)

testListConsCompo0 = TestCase (do
                                a <- runSingle "(cons 1 '())"
                                let b = "(1)"
                                assertEqual "Simple cons evaluation" b a)

testListCarCompo1 = TestCase (do
                                a <- runSingle "(car (cons 1 '()))"
                                let b = "1"
                                assertEqual "Should evaluate car on a cons List" b a)

testListCdrCompo1 = TestCase (do
                                a <- runSingle "(cdr (cons 1 '()))"
                                let b = "()"
                                assertEqual "Should evaluate cdr on a List with only one element" b a)

testListCdrCompo2 = TestCase (do
                                a <- runSingle "(cdr (cons 1 '(2 3)))"
                                let b = "(2 3)"
                                assertEqual "Should construct cdr on a List with several elements" b a)

testImproperList = TestCase (do
                                a <- runSingle "(cons 1 2)"
                                let b = "(1 . 2)"
                                assertEqual "Basic construction of an ImproperList" b a)

testImproperList2 = TestCase (do
                                a <- runSingle "(cons '() 1)"
                                let b = "(() . 1)"
                                assertEqual "another construction of improperList, with a list as first argument" b a)

testImproperList3 = TestCase (do
                                a <- runSingle "(car '(1 . 2))"
                                let b = "1"
                                assertEqual "construciton with quote" b a)

testImproperCar = TestCase (do
                                a <- runSingle "(car (cons '(3 . 4) 1))"
                                let b = "(3 . 4)"
                                assertEqual "car on improperList" b a)

testImproperCdr = TestCase (do
                                a <- runSingle "(cdr (cons '(1 . 2) 2))"
                                let b = "2"
                                assertEqual "ImproperList cdr" b a)


testListHard = TestCase (do
                                a <- runSingle "(cons '(1) '(2 3))"
                                let b = "((1) 2 3)"
                                assertEqual "sublist construction" b a)

testCondBasic = TestCase (do
                                a <- runSingle "(cond (#f 1) (#t '(1 1)))"
                                let b = "(1 1)"
                                assertEqual "(cond (#f 1) (#t '(1 1)))" b a)

testCondBasic2 = TestCase (do
                                a <- runSingle "(cond ((eq? 'foo 'foo) 1) (#t 4))"
                                let b = "1"
                                assertEqual "(cond ((eq? 'foo 'foo) 1) (#t 4))" b a)

testEqBasic = TestCase (do
                                a <- runSingle "(eq? 'foo 'foo)"
                                let b = "#t"
                                assertEqual "Basic equality" b a)

testQuote' = TestCase (do
                                a <- runSingle "(quote (1 (quote 3)))"
                                let b = "(1 '3)"
                                assertEqual "quote with quote" b a)

testQuote = TestCase (do
                                a <- runSingle "'(1 car '(2))"
                                let b = "(1 car '(2))"
                                assertEqual "basic quote" b a)

testEq = TestCase (do
                                a <- runSingle "(eq? 'foo (car '(foo bar)))"
                                let b = "#t"
                                assertEqual "more sophisticated equality" b a)

testRunParser :: Test
testRunParser = TestList [
    "parsing a character"           ~: testParserChar,
    "parsing a digit"               ~: testParserDigit,
    "parsing some character"        ~: testParserHas,
    "not parsing some character"    ~: testParserHas',
    "eval a string"                 ~: testEvalString,
    "eval a bool"                   ~: testEvalBoolTrue,
    "eval another bool"             ~: testEvalBoolFalse,
    "eval a number"                 ~: testEvalNumber,
    "eval a negative number"        ~: testEvalNegNumber,
    "eval an atom"                  ~: testEvalAtom,
    "eval quote primitive"          ~: testListCompo1,
    "eval a basic substraction"     ~: testListCompo2,
    "eval cons on list"             ~: testListCompo3,
    "eval cons on list 2"           ~: testListCompo4,
    "eval car on list"              ~: testListCarCompo1,
    "eval cdr on list"              ~: testListCdrCompo1,
    "eval cdr 2 on list"            ~: testListCdrCompo2,
    "eval basic condition"          ~: testCondBasic,
    "eval basic cond 2"             ~: testCondBasic2,
    "eval basic equal?"             ~: testEqBasic,
    "eval sophisticated equal?"     ~: testEq,
    "eval basic cons"               ~: testListConsCompo0,
    "eval list hard"                ~: testListHard,
    "eval improperlist"             ~: testImproperList,
    "eval improperlist 2"           ~: testImproperList2,
    "eval improperList 3"           ~: testImproperList3,
    "eval improperlist car"         ~: testImproperCar,
    "eval improperlist cdr"         ~: testImproperCdr,
    "eval quote"                    ~: testQuote,
    "eval quote 2"                  ~: testQuote'
{-
    "eval invalid type"             ~: testEvalBadForm,
    "eval bad comparison"           ~: testCompBadTypes,
    "eval unknown prim"             ~: testUnknownPrim,
    "eval eq invalid nb of args"    ~: testEqNumArgs,
    "eval car invalid case"         ~: testCarNumArgs,
    "eval car invalid case 2"       ~: testCarInvalidType,
    "eval cdr invalid case"         ~: testCdrNumArgs,
    "eval cdr invalid case 2"       ~: testCdrInvalidType,
    "eval cons invalid case"        ~: testConsNumArgs-}
    ]

import Test.HUnit

import Lib
import CombinedParsers
import ParserLib
import Eval
import Tokens

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

testEvalString = TestCase $ assertEqual "Just return the string"
                (Right $ Tokens.String "blabla") (eval $ Tokens.String "blabla")

testEvalBoolTrue = TestCase $ assertEqual "Just return the boolean True"
                (Right $ Tokens.Bool True ) (eval $ Tokens.Bool True)
testEvalBoolFalse =  TestCase $ assertEqual "Just return the boolean False"
                (Right $ Tokens.Bool False) (eval $ Tokens.Bool False)
testEvalNumber = TestCase $ assertEqual "Just return the number"
                (Right $ Tokens.Number 0) (eval $ Tokens.Number 0)
testEvalNegNumber =  TestCase $ assertEqual "Just return the negative number"
                (Right $ Tokens.Number (-1)) (eval $ Tokens.Number (-1))
testEvalAtom = TestCase $ assertEqual "Just return the atom"
                (Right $ Tokens.Atom "word") (eval $ Tokens.Atom "word")

testListCompo1 = TestCase $ assertEqual "basic quote prim"
                (Right $ Tokens.Number 1) (eval $ Tokens.List [Tokens.Atom "quote", Tokens.Number 1])

testListCompo2 = TestCase $ assertEqual "Should compute the result"
                (Right $ Tokens.Number 3) (eval $ Tokens.List [Tokens.Atom "-", Tokens.Number 4, Tokens.Number 1])

testListCompo3 = TestCase $ assertEqual "Should construct a proper List"
                (Right $ Tokens.List [Tokens.Number 4])
                (eval $ Tokens.List [Tokens.Atom "cons", Tokens.Number 4, Tokens.List []])

testListCompo4 =  TestCase $ assertEqual "Should construct a proper List"
                (Right $ Tokens.List [Tokens.String "a", Tokens.String "b"])
                (eval $ Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.List [Tokens.Atom "cons", Tokens.String "b", Tokens.List []]])

testListConsCompo0 = TestCase $ assertEqual "Simple cons evaluation"
                (Right $ Tokens.List [Tokens.String "a"])
                (eval $ Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.List []])

testListCarCompo1 = TestCase $ assertEqual "Should evaluate car on a cons List"
                (Right $ Tokens.String "a")
                (eval $ Tokens.List [Tokens.Atom "car", Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.List []]])

testListCdrCompo1 = TestCase $ assertEqual "Should evaluate cdr on a List with only one element"
                (Right Tokens.Nil)
                (eval $ Tokens.List [Tokens.Atom "cdr", Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.List []]])

testListCdrCompo2 = TestCase $ assertEqual "Should construct cdr on a List with several elements"
                (Right $ Tokens.List [Tokens.String "b", Tokens.String "c"])
                (eval $ Tokens.List [Tokens.Atom "cdr",
                        Tokens.List [Tokens.Atom "quote", Tokens.List [Tokens.String "a", Tokens.String "b", Tokens.String "c"]]])

testImproperList = TestCase $ assertEqual "Basic construction of an ImproperList"
                (Right $ Tokens.ImproperList [Tokens.String "a"] (Tokens.String "b"))
                (eval $ Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.String "b"])


testImproperList2 = TestCase $ assertEqual "Represent: (cons '() 1)"
                    (Right $ Tokens.ImproperList [Tokens.List[]] (Tokens.Number 1))
                    (eval $ Tokens.List[Tokens.Atom "cons", Tokens.List [Tokens.Atom "quote", Tokens.List[]], Tokens.Number 1])

testImproperListCar = TestCase $ assertEqual "Represents: (car (cons '(a b) 1))"
                (Right $ Tokens.Number 1)
                (run "(car (cons '(a b) 1))")

testImproperListCdr = TestCase $ assertEqual "Represents: (cdr (cons '(a b))"
                (Right $ Tokens.String "b")
                (eval $ Tokens.List [Tokens.Atom "cdr",
                        Tokens.List[Tokens.Atom "cons", Tokens.String "a", Tokens.String "b"]])


testListHard = TestCase $ assertEqual "Represents: (cons '(1) '(2 3))"
                (Right $ Tokens.List[Tokens.List [Tokens.Number 1], Tokens.Number 2, Tokens.Number 3])
                (eval $ Tokens.List[Tokens.Atom "cons", Tokens.List[Tokens.Atom "quote", Tokens.List[Tokens.Number 1]], Tokens.List[Tokens.Atom "quote", Tokens.List[Tokens.Number 2, Tokens.Number 3]]])

testCondBasic = TestCase $ assertEqual "Represent:  (cond (#f 1) (#t (+ 1 1)))"
                (Right $ Number 2)
                (eval $ Tokens.List[Tokens.Atom "cond", Tokens.List[Tokens.Bool False, Tokens.Number 1], Tokens.List[Tokens.Bool True, Tokens.List[Tokens.Atom "+", Tokens.Number 1, Tokens.Number 1]]])

testEqBasic = TestCase $ assertEqual "(eq? 1 1)"
                (Right $ Tokens.Bool True)
                (eval $ Tokens.List[Tokens.Atom "eq?", Tokens.Number 1, Tokens.Number 1])

testQuote = TestCase $ assertEqual "Represents: '(1 car '(2))" -- Shouldn't evaluate anything, just return args.
                (Right $ Tokens.List[Tokens.Number 1, Tokens.Atom "car", Tokens.List[Tokens.Atom "quote", Tokens.List[Tokens.Number 2]]])
                (run "'(1 car '(2))")

testEq = TestCase $ assertEqual "Represent: (eq? 'foo (car '(foo bar)))"
                (Right $ Tokens.Bool True)
                (eval $ Tokens.List[Tokens.Atom "eq?", Tokens.List[Tokens.Atom "quote", Tokens.Atom "foo"], Tokens.List[Tokens.Atom "car", Tokens.List[Tokens.Atom "quote", Tokens.List[Tokens.Atom "foo", Tokens.Atom "bar"]]]])

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
    "eval basic equal?"             ~: testEqBasic,
    "eval sophisticated equal?"     ~: testEq,
    "eval basic cons"               ~: testListConsCompo0,
    "eval list hard"                ~: testListHard,
    "eval improperlist"             ~: testImproperList,
    "eval improperlist 2"           ~: testImproperList2,
    "eval improperlist car"         ~: testImproperListCar,
    "eval improperlist cdr"         ~: testImproperListCdr,
    "eval quote"                    ~: testQuote,

    "eval invalid type"             ~: testEvalBadForm,
    "eval bad comparison"           ~: testCompBadTypes,
    "eval unknown prim"             ~: testUnknownPrim,
    "eval eq invalid nb of args"    ~: testEqNumArgs,
    "eval car invalid case"         ~: testCarNumArgs,
    "eval car invalid case 2"       ~: testCarInvalidType,
    "eval cdr invalid case"         ~: testCdrNumArgs,
    "eval cdr invalid case 2"       ~: testCdrInvalidType,
    "eval cons invalid case"        ~: testConsNumArgs
    ]

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
                (runParser char "Sheesh") (Just 'S')
testParserDigit = TestCase $ assertEqual "Get a digit if any"
                (runParser digit "0213q") (Just '0')
testParserHas = TestCase $ assertEqual "Should get first character if predicate"
                (runParser (has 'c') "char") (Just 'c')
testParserHas' = TestCase $ assertEqual "Should not get first character if pred"
                (runParser (has 'd') "char") Nothing

testEvalString = TestCase $ assertEqual "Just return the string"
                (eval $ Tokens.String "blabla") (Right $ Tokens.String "blabla")

testEvalBoolTrue = TestCase $ assertEqual "Just return the boolean True"
                (eval $ Tokens.Bool True) (Right $ Tokens.Bool True)
testEvalBoolFalse =  TestCase $ assertEqual "Just return the boolean False"
                (eval $ Tokens.Bool False) (Right $ Tokens.Bool False)
testEvalNumber = TestCase $ assertEqual "Just return the number"
                (eval $ Tokens.Number 0) (Right $ Tokens.Number 0)
testEvalNegNumber =  TestCase $ assertEqual "Just return the negative number"
                (eval $ Tokens.Number (-1)) (Right $ Tokens.Number (-1))
testEvalAtom = TestCase $ assertEqual "Just return the atom"
                (eval $ Tokens.Atom "word") (Right $ Tokens.Atom "word")

testListCompo1 = TestCase $ assertEqual "basic quote prim"
                (eval $ Tokens.List [Tokens.Atom "quote", Tokens.Number 1]) (Right $ Tokens.Number 1)

testListCompo2 = TestCase $ assertEqual "Should compute the result"
                (eval $ Tokens.List [Tokens.Atom "-", Tokens.Number 4, Tokens.Number 1]) (Right $ Tokens.Number 3)

testListCompo3 = TestCase $ assertEqual "Should construct a proper List"
                (eval $ Tokens.List [Tokens.Atom "cons", Tokens.Number 4, Tokens.Nil])
                (Right $ Tokens.List [Tokens.Number 4, Tokens.Nil])

testListCompo4 =  TestCase $ assertEqual "Should construct a nested proper List"
                (eval $ Tokens.List [Tokens.Atom "cons", Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.Nil], Tokens.Nil])
                (Right $ Tokens.List [Tokens.List [Tokens.String "a", Tokens.String "b", Tokens.Nil], Tokens.Nil])

testListCarCompo1 = TestCase $ assertEqual "Should evaluate car on a cons List"
                (eval $ Tokens.List [Tokens.Atom "car", Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.Nil]])
                (Right $ Tokens.String "a")

testListCdrCompo1 = TestCase $ assertEqual "Should evaluate cdr on a List with only one element"
                (eval $ Tokens.List [Tokens.Atom "cdr", Tokens.List [Tokens.Atom "cons", Tokens.String "a", Tokens.Nil]])
                (Right Tokens.Nil)

testListCdrCompo2 = TestCase $ assertEqual "Should construct cdr on a List with several elements"
                (eval $ Tokens.List [Tokens.Atom "cdr",
                        Tokens.List [Tokens.Atom "quote", Tokens.List [Tokens.String "a", Tokens.String "b", Tokens.String "c"]]]) --it does not construct an ImproperList because of quote
                (Right $ Tokens.List [Tokens.String "b", Tokens.String "c", Tokens.Nil]) -- but we should add nil at the end anyways.

testCondBasic = TestCase $ assertEqual "Represent:  (cond (#f 1) (#t (+ 1 1)))"
                (eval $ Tokens.List[Tokens.Atom "cond", Tokens.List[Tokens.Bool False, Tokens.Number 1], Tokens.List[Tokens.Bool True, Tokens.List[Tokens.Atom "+", Tokens.Number 1, Tokens.Number 1]]])
                (Right $ Number 2)

testEqBasic = TestCase $ assertEqual "(eq? 1 1)"
                (eval $ Tokens.List[Tokens.Atom "eq?", Tokens.Number 1, Tokens.Number 1])
                (Right $ Tokens.Bool True)

testEq = TestCase $ assertEqual "Represent: (eq? 'foo (car '(foo bar)))"
                (eval $ Tokens.List[Tokens.Atom "eq?", Tokens.List[Tokens.Atom "quote", Tokens.Atom "foo"], Tokens.List[Tokens.Atom "car", Tokens.List[Tokens.Atom "quote", Tokens.List[Tokens.Atom "foo", Tokens.Atom "bar"]]]])
                (Right $ Tokens.Bool True)

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
module TestEvalInvalidCases where
import Test.HUnit

import Lib
import CombinedParsers
import ParserLib
import Eval
import Tokens
    ( Token(Number, List, Atom, String, Nil),
      RuntimeError(TypeMismatch, BadSpecialForm, Default, NotFunction, NumArgs) )
import TestLib

testEvalBadForm = TestCase $ assertEqual "shouldn't match any patterns."
                (eval $ Tokens.List []) (Left $ BadSpecialForm $ Tokens.List [])

testCompBadTypes = TestCase $ assertEqual "unable to compare number and string."
                (eval $ Tokens.List[Tokens.Atom "<", Tokens.String "lol", Tokens.Number (-1)])
                (Left $ TypeMismatch "number" (Tokens.String "lol"))

testUnknownPrim = TestCase $ assertEqual "unable to locate such primitive."
                (eval $ Tokens.List[Tokens.Atom "lol", Tokens.Nil])
                (Left $ NotFunction "lol")

testEqNumArgs = TestCase $ assertEqual "eq waits for 2 arguments."
                (eval $ Tokens.List[Tokens.Atom "eq?", Tokens.Number 1])
                (Left $ NumArgs 2 [Tokens.Number 1])

testCarNumArgs = TestCase $ assertEqual "car waits for 1 list."
                (eval $ Tokens.List[Tokens.Atom "car", Tokens.List[Tokens.Number 1], Tokens.Number 1])
                (Left $ NumArgs 1 [Tokens.List[Tokens.Number 1], Tokens.Number 1])

testCarInvalidType = TestCase $ assertEqual "car waits for 1 list || improperlist."
                (eval $ Tokens.List[Tokens.Atom "car", Tokens.Number 1])
                (Left $ NumArgs 1 [Tokens.Number 1])

testCdrNumArgs = TestCase $ assertEqual "cdr waits for 1 list."
                (eval $ Tokens.List[Tokens.Atom "cdr", Tokens.List[Tokens.Number 1], Tokens.Number 1])
                (Left $ NumArgs 1 [Tokens.List[Tokens.Number 1], Tokens.Number 1])

testCdrInvalidType = TestCase $ assertEqual "cdr waits for 1 list || improperlist."
                (eval $ Tokens.List[Tokens.Atom "cdr", Tokens.Number 1])
                (Left $ NumArgs 1 [Tokens.Number 1])

testConsNumArgs = TestCase $ assertEqual "cons waits for 2 elements."
                (eval $ Tokens.List[Tokens.Atom "cons", Tokens.List[Tokens.Number 1]])
                (Left $ NumArgs 2 [Tokens.List[Tokens.Number 1]])


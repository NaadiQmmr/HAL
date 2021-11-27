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
                (Right $ Nil)
                (eval $ Tokens.List [])

testCompBadTypes = TestCase $ assertEqual "unable to compare number and string."
                (Left $ TypeMismatch "number" (Tokens.String "lol"))
                (eval $ Tokens.List[Tokens.Atom "<", Tokens.String "lol", Tokens.Number (-1)])

testUnknownPrim = TestCase $ assertEqual "unable to locate such primitive."
                (Left $ NotFunction "lol")
                (eval $ Tokens.List[Tokens.Atom "lol", Tokens.Nil])

testEqNumArgs = TestCase $ assertEqual "eq waits for 2 arguments."
                (Left $ NumArgs 2 [Tokens.Number 1])
                (eval $ Tokens.List[Tokens.Atom "eq?", Tokens.Number 1])

testCarNumArgs = TestCase $ assertEqual "car waits for 1 list."
                (Left $ NumArgs 1 [Tokens.List[Tokens.Number 1], Tokens.Number 1])
                (eval $ Tokens.List[Tokens.Atom "car", Tokens.List[Tokens.Number 1], Tokens.Number 1])

testCarInvalidType = TestCase $ assertEqual "car waits for 1 list || improperlist."
                (Left $ TypeMismatch "pair" $ Tokens.Number 1)
                (eval $ Tokens.List[Tokens.Atom "car", Tokens.Number 1])

testCdrNumArgs = TestCase $ assertEqual "cdr waits for 1 list."
                (Left $ TypeMismatch "pair" $ Tokens.List[Tokens.Number 1])
                (eval $ Tokens.List[Tokens.Atom "cdr", Tokens.List[Tokens.Number 1], Tokens.Number 1])

testCdrInvalidType = TestCase $ assertEqual "cdr waits for 1 list || improperlist."
                (Left $ TypeMismatch "pair" $ Tokens.Number 1)
                (eval $ Tokens.List[Tokens.Atom "cdr", Tokens.Number 1])

testConsNumArgs = TestCase $ assertEqual "cons waits for 2 elements."
                (Left $ NumArgs 2 [Tokens.List[Tokens.Number 1]])
                (eval $ Tokens.List[Tokens.Atom "cons", Tokens.List[Tokens.Number 1]])


module TestLib where
import Test.HUnit

import Lib
import CombinedParsers
import ParserLib
import Eval
import Tokens


instance Eq Token where
        Bool a == Bool b = a == b
        String a == String b = a == b
        Atom a == Atom b = a == b
        Number a == Number b = a == b
        List a == List b = a == b
        ImproperList list_a a == ImproperList list_b b = list_a == list_b && a == b
        ImproperList list_a Nil == List list_b = list_a == list_b
        Primitive a _ == Primitive b _ = a == b
        --Lambda _ f == Lambda _ f' == f == f'
        Nil == Nil = True
        List [Nil] == Nil = True
        _ == _ = False

instance Eq RuntimeError where
        NumArgs int a == NumArgs int' a' = int == int' && a == a'
        TypeMismatch s t == TypeMismatch s' t' = s == s' && t == t'
        BadSpecialForm t == BadSpecialForm t' = t == t'
        NotFunction s == NotFunction s' = s == s'
        UnboundVar s == UnboundVar s' = s == s'
        Default s == Default s' = s == s'
        _ == _ = False
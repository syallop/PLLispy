{-# LANGUAGE OverloadedStrings #-}
module PLLispy.Test.ExprSpec where

import PL
import PL.Test.Expr
import PL.Test.Expr.BigLam
import PL.Test.Expr.Boolean
import PL.Test.Expr.Function
import PL.Test.Expr.Lam
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union

import PLLispy
import PLLispy.Expr
import PLLispy.Type

import Data.Text
import qualified Data.Text as Text
import Data.Monoid

import Test.Hspec

spec
  :: Spec
spec = parserSpec lispySources lispyParser
  where
    lispyParser = toParser $ expr var (typ tyVar) tyVar

    lispySources :: TestExprSources
    lispySources = TestExprSources
      { _lamTestCases = TestLamSources
          { _singleLamTestCase  = "λFoo (0)"
          , _nestedLamTestCase  = "λFoo (λBar 1)"
          , _chainedLamTestCase = "λFoo Bar Baz 2"
          }

      , _bigLamTestCases = TestBigLamSources
          { _singleBigLamTestCase = "ΛKIND λ(?0) 0"
          }

      , _booleanTestCases = TestBooleanSources
          { _andTestCase = Text.unlines
              ["λBool λBool (CASE 0"
              ,"                  ("   -- case matches
              ,"                    (" -- Non-default matches
              ,"                      (|"<>falsePatText<>" "<>falseTermText<>")" -- first (and only) match
              ,"                    )"
              ,"                    (" -- default match
              ,"                      (CASE 1"
              ,"                            (" -- case matches
              ,"                              (" --Non-default matches
              ,"                                (|"<>falsePatText<>" "<>falseTermText<>")" -- first (and only) match
              ,"                              )"
              ,"                              (" -- default match
              ,"                                "<>trueTermText<>"" -- default is the true expr
              ,"                              )"
              ,"                            )"
              ,"                      )"
              ,"                    )"
              ,"                  )"
              ,"            )"
              ]
          }

      , _naturalTestCases = TestNaturalSources
          { _subTwoTestCase = Text.unlines
              ["λNat (CASE 0"
              ,"         (| (+1 (?)) (0))"   -- Match successor
              ,"         (+0 (*) (*) Nat)" -- Default branch: zero
              ,"     )"
              ]
          }

      , _sumTestCases = TestSumSources
          { _sumThreeTestCase = Text.unlines
              ["λ(+ Nat Bool Nat) (CASE 0"
              ,"                   (| (+ 0 +1 ?)  (0))"
              ,"                   (| (+ 0 +0 *)  (+0 (*) (*) Nat))"
              ,"                   (| (+ 1 +0 *)  (+0 (*) (*) Nat))"
              ,"                   (| (+ 1 +1 *)  (@ (λNat (+1 (0) (*) Nat))  (+0 (*) (*) Nat)))"
              ,"                   (| (+ 2 +1 ?)  (+0 (*) (*) Nat))"
              ,"                   (| (+ 2 +0 *)  (@ (λNat (+1 (0) (*) Nat))  (+0 (*) (*) Nat)))"
              ,"                 )"
              ]
          }

      , _productTestCases = TestProductSources
          { _productThreeTestCase = Text.unlines
              ["λ(* Nat Bool Nat) (CASE 0"
              ,"                    (| (* (+0 (*)) (?) (+0 (*))) (0))"
              ,"                    (| (* (?)      (?) (+0 (*))) (0))"
              ,""
              ,"                    (+0 (*) (*) (*))"
              ,"                  )"
              ]
          }

      , _unionTestCases = TestUnionSources
          { _unionTwoTestCase = Text.unlines
              ["λ(∪ Bool Nat) (CASE 0"
              ,"                (| (∪ Nat  (+0 (*))) (+0 (*) (*) (*)))"
              ,"                (| (∪ Nat  (+1 ?))   (+1 (*) (*) (*)))"
              ,"                (| (∪ Bool (+1 (*))) (+1 (*) (*) (*)))"
              ,""
              ,"                (+0 (*) (*) (*))"
              ,"              )"
              ]
          }

      , _functionTestCases = TestFunctionSources
          { _idTestCase    = "Λ KIND λ?0 (0)"
          , _constTestCase = "Λ KIND (ΛKIND (λ?1 (λ?0 (1))))"
          , _applyTestCase = "Λ KIND (ΛKIND (λ(→ ?1 ?0) (λ?1 (@1 (0)))))"
          }
      }


falseTermText, trueTermText, falsePatText, truePatText :: Text
falseTermText = "+0 (*) (*) (*)"
trueTermText  = "+1 (*) (*) (*)"
falsePatText  = "(+0 (*))"
truePatText   = "(+1 (*))"

zTermText, sTermText, zPatText :: Text
zTermText  = "+0(*) (*) Nat"
sTermText  = "λNat (+1 0 (*) Nat)"
zPatText   = "+0(*)"
sPatText p = "+1"<>p


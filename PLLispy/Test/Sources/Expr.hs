{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLLispy.Test.Sources.Expr
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Exports test source code fragments for the PL test cases.
This might be used for:
- This libraries test suite to ensure the grammar/ printers/ parsers are behaving
- External libraries who wish to do something with valid source code fragments,
  such as displaying examples.
-}
module PLLispy.Test.Sources.Expr
  ( sources
  )
  where

import PL
import PL.Expr
import PL.Var
import PL.TyVar
import PL.Type
import PL.Test.Expr
import PL.Test.Expr.BigLam
import PL.Test.Expr.Boolean
import PL.Test.Expr.Function
import PL.Test.Expr.Lam
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union

import Data.Text
import qualified Data.Text as Text

sources :: TestExprSources
sources = TestExprSources
  { _lamTestCases = TestLamSources
      { _singleLamTestCase  = "λFoo (0)"
      , _nestedLamTestCase  = "λFoo (λBar 1)"
      , _chainedLamTestCase = "λFoo Bar Baz 2"
      }

  , _bigLamTestCases = TestBigLamSources
      { _singleBigLamTestCase = "ΛKIND λ(?0) 0"
      }

  , _booleanTestCases = TestBooleanSources
      { _andTestCase =
          "λBool λBool (CASE 0\n\
          \              (| (+0 (*)) (+0 (*) (*) (*)))\n\
          \              (CASE 1\n\
          \               (| (+0 (*)) (+0 (*) (*) (*)))\n\
          \               (+1 (*) (*) (*))\n\
          \              )\n\
          \            )"
      }

  , _naturalTestCases = TestNaturalSources
      { _subTwoTestCase =
          "λNat (CASE 0\n\
          \       (| (+1 (+1 ?)) (0))\n\
          \       (+0 (*) (*) Nat)\n\
          \     )"
      }

  , _sumTestCases = TestSumSources
      { _sumThreeTestCase =
          "λ(+ Nat Bool Nat) (CASE 0\n\
          \                     (| (+0 (+1 ?)) (0))\n\
          \                     (| (+0 (+0 *)) (+0 (*) (*) Nat))\n\
          \                     (| (+1 (+0 *)) (+0 (*) (*) Nat))\n\
          \                     (| (+1 (+1 *)) (@ (λNat (+1 (0) (*) Nat)) (+0 (*) (*) Nat)))\n\
          \                     (| (+2 (+1 ?)) (+0 (*) (*) Nat))\n\
          \                     (| (+2 (+0 *)) (@ (λNat (+1 (0) (*) Nat)) (+0 (*) (*) Nat)))\n\
          \                    )"
      }

  , _productTestCases = TestProductSources
      { _productThreeTestCase =
          "λ(* Nat Bool Nat) (CASE 0\n\
          \                    (| (* (+0 (*)) (?) (+0 (*))) (0))\n\
          \                    (| (* (?)      (?) (+0 (*))) (0))\n\
          \                    (+0 (*) (*) (*))\n\
          \                  )"
      }

  , _unionTestCases = TestUnionSources
      { _unionTwoTestCase =
          "λ(∪ Bool Nat) (CASE 0\n\
          \                (| (∪ Nat  (+0 (*))) (+0 (*) (*) (*)))\n\
          \                (| (∪ Nat  (+1 ?))   (+1 (*) (*) (*)))\n\
          \                (| (∪ Bool (+1 (*))) (+1 (*) (*) (*)))\n\
          \                (+ 0 (*) (*) (*))\n\
          \              )"
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
zTermText  = "+0 (*) (*) Nat"
sTermText  = "λNat (+1 0 (*) Nat)"
zPatText   = "+0 (*)"
sPatText p = "+1 "<>p


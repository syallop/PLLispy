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
import PL.Test.Expr.Binding
import PL.Test.Expr.Function
import PL.Test.Expr.Maybe
import PL.Test.Expr.Lam
import PL.Test.Expr.List
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union
import PL.Test.Expr.SelfTypes

import Data.Text
import qualified Data.Text as Text

sources :: TestExprSources
sources = TestExprSources
  { _lamTestCases = TestLamSources
      { _singleLamTestCase  =
          "\"An anonymous function which accepts any expression with the type Bool\n\
          \and binds it in the 0th position to be returned unchanged.\"\n\n\
          \λBool (0)"

      , _nestedLamTestCase  =
          "\"Accept multiple arguments by nesting lambdas. Bindings may refer\n\
          \to any expression bound in a lambda above them.\"\n\n\
          \λBool (λNat 1)"

      , _chainedLamTestCase =
          "\"Instead of nesting lambdas to accept multiple arguments they can\n\
          \be given to a single lambda.\"\n\n\
          \λBool Nat Unit 2"
      }

  , _bigLamTestCases = TestBigLamSources
      { _singleBigLamTestCase =
          "\"An anonymous function which accepts any type with the kind KIND\n\
          \, binds it in the 0th position for type arguments\n\
          \, accepts any expression with the provided 0th type\n\
          \and binds it in the 0th position for expression arguments to be\n\
          \returned unchanged.\"\n\n\
          \ΛKIND λ(?0) 0"
      }

  , _booleanTestCases = TestBooleanSources
      { _andTestCase =
          "\"Boolean and. Returns a true expression if and only if both arguments\n\
          \are true. Otherwise false.\"\n\n\
          \λBool λBool (CASE 0\n\
          \              (| (+0 (*)) (+0 (*) (*) (*)))\n\
          \              (CASE 1\n\
          \               (| (+0 (*)) (+0 (*) (*) (*)))\n\
          \               (+1 (*) (*) (*))\n\
          \              )\n\
          \            )"
      }

  , _bindingTestCases = TestBindingSources
      { _bindingTestCase =
          "\"Variables are refered to by counting the number of lambda away they are\n\
          \abstracted from. This lambda will return the Bool value it is passed\n\
          \as it is zero bindings away.\"\n\n\
          \λUnit 0"

      , _buriedBindingTestCase =
         "\"Variables are referenced by the number of lambdas are away they were bound.\n\
         \These indexes are adjusted so they point to their original value when\
         \reduction causes the number of intermediate lambdas to change.\n\
         \In this expression the binding of 1 will become 2 under a reduction step.\"\n\n\
         \λBool (@ (λ(-> Bool Bool) (λBool 1)) (λBool 1))"

      , _doubleBuriedBindingTestCase =
         "\"Variables are referenced by the number of lambdas are away they were bound.\n\
         \These indexes are adjusted so they point to their original value when\
         \reduction causes the number of intermediate lambdas to change.\n\
         \In this expression the binding of 1 will become 3 under a reduction step.\"\n\n\
         \λBool (@ (λ(-> Bool Bool) (λBool (λBool 2))) (λBool 1))"

      , _buriedBindingsDontMoveTestCase =
         "\"Variables are referenced by the number of lambdas are away they were bound.\n\
         \These indexes are adjusted so they point to their original value when\
         \reduction causes the number of intermediate lambdas to change.\n\
         \In this expression the binding of 0 remains 0 away under a reduction step.\"\n\n\
         \λBool (@ (λBool 0) 0)"
      }

  , _naturalTestCases = TestNaturalSources
      { _subTwoTestCase =
          "\"Subtract two from a natural number, defaulting to 0 if the natural\n\
          \is too small.\"\n\n\
          \λNat (CASE 0\n\
          \       (| (+1 (+1 ?)) (0))\n\
          \       (+0 (*) (*) Nat)\n\
          \     )"
      }

  , _sumTestCases = TestSumSources
      { _sumThreeTestCase =
          "\"A function displaying matching on a sum of three types to do nothing\n\
          \of any obvious use.\"\n\n\
          \λ(+ Nat Bool Nat) (CASE 0\n\
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
          "\"A function displaying matching on a product of three types to do nothing\n\
          \of any obvious use.\"\n\n\
          \λ(* Nat Bool Nat) (CASE 0\n\
          \                    (| (* (+0 (*)) (?) (+0 (*))) (0))\n\
          \                    (| (* (?)      (?) (+0 (*))) (0))\n\
          \                    (+0 (*) (*) (*))\n\
          \                  )"
      }

  , _unionTestCases = TestUnionSources
      { _unionTwoTestCase =
          "\"A function displaying matching on a union of two types to do nothing\n\
          \of any obvious use.\"\n\n\
          \λ(∪ Bool Nat) (CASE 0\n\
          \                (| (∪ Nat  (+0 (*))) (+0 (*) (*) (*)))\n\
          \                (| (∪ Nat  (+1 ?))   (+1 (*) (*) (*)))\n\
          \                (| (∪ Bool (+1 (*))) (+1 (*) (*) (*)))\n\
          \                (+ 0 (*) (*) (*))\n\
          \              )"
      }

  , _functionTestCases = TestFunctionSources
      { _idTestCase =
          "\"The identity function takes any type of kind KIND,\n\
          \any expression of that type and returns the expression unaltered.\"\n\n\
          \Λ KIND λ?0 (0)"

      , _constTestCase =
          "\"The const function takes two types of kind KIND\n\
          \any two expressions of the respective types and returns the first\n\
          \argument unaltered.\"\n\n\
          \Λ KIND (ΛKIND (λ?1 (λ?0 (1))))"

      , _applyTestCase =
          "\"The apply function takes two types of kind KIND\n\
          \a function expression between the two types, a value of the first type\n\
          \and applies the value to the function.\"\n\
          \Λ KIND (ΛKIND (λ(→ ?1 ?0) (λ?1 (@1 (0)))))"
      }

  , _maybeTestCases = TestMaybeSources
      { _defaultNatTestCase =
          "\"Accept a value of type Maybe Nat, if it's nothing default to zero,\n\
          \otherwise unwrap the number\"\n\
          \λ(/@Maybe Nat) (CASE 0\n\
          \  (| (+0 (*)) (+0 (*) (*) Nat))\n\
          \  (| (+1 ?)   (0))\n\
          \)"
      }

  , _listTestCases = TestListSources
      { _emptyListTestCase =
          "\"The empty list value is constructed by passing a type to this function.\n\
          \An list value is a sum type of either:\n\
          \ - The empty product indicating the end of the list\n\
          \ - Or the product of:\n\
          \   - Whatever type the list should contain\n\
          \   - And a recursive occurance of the list of the given type.\"\n\n\
          \ΛKIND (+0 (*) (*) (* ?0 (/@List ?0)))"
      }

  , _selfTypeTestCases = TestSelfTypeSources
      { _selfTypesCanBeMentionedTestCase =
          "\"Self types can be mentioned anonymously wherever types are allowed.\n\
          \This sum expression contains a Nat-like type as its second (unused) alternative.\"\n\n\
          \+0 (*) (*) (μKIND (+ (*) %))"
      , _selfTypesCanBeReturnedTestCase =
          "\"Self types can be returned from functions. This Lambda expression\n\
          \accepts an expression with a Nat-like type and returns it.\"\n\n\
          \λ(μKIND (+ (*) %)) 0"

      , _selfTypesCanBeConstructedTestCase =
        "\"Self types can be constructed from conforming expressions.\n\
        \This expression constructs a Zero-like expression and passes it\n\
        \to a function expecting a Nat-like type.\"\n\n\
        \@ (λ(μKIND (+ (*) %)) 0)\n\
        \  (+0 (*)\n\
        \      (*) (μKIND (+ (*) %)))"

      , _nestedSelfTypesCanBeConstructedTestCase =
        "\"Nested self types can be constructed from conforming expressions.\n\
        \This expression constructs a One-like expression and passes it\n\
        \to a function expecting a Nat-like type.\"\n\n\
        \@ (λ(μKIND (+ (*) %)) 0)\n\
        \  (+1 (+0 (*)\n\
        \          (*) (μKIND (+ (*) %)))\n\
        \      (*) (μKIND (+ (*) %)))"

      , _selfTypesCanBeDeconstructedTestCase =
          "\"Self types can be deconstructed by matching on their definitions.\"\n\
          \λ(μKIND (+ (*) %))\n\
          \  (CASE 0\n\
          \    (| (+0 ?) (+0 (*) (*) (*)))\n\
          \    (| (+1 ?) (+1 (*) (*) (*)))\n\
          \  )"
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


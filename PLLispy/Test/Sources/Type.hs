{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLLispy.Test.Sources.Type
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Exports test source code fragments for the PL test cases.
This might be used for:
- This libraries test suite to ensure the grammar/ printers/ parsers are behaving
- External libraries who wish to do something with valid source code fragments,
  such as displaying examples.
-}
module PLLispy.Test.Sources.Type
  ( sources
  )
  where

import PL
import PL.Expr
import PL.Var
import PL.TyVar
import PL.Type

import PL.Test.Type
import PL.Test.Type.Arrow
import PL.Test.Type.BigArrow
import PL.Test.Type.Product
import PL.Test.Type.Sum
import PL.Test.Type.TypeBinding
import PL.Test.Type.TypeLam
import PL.Test.Type.Union
import PL.Test.Type.Named

import PL.Test.TypeTestCase

import Data.Text
import qualified Data.Text as Text

-- TODO: Write some tests, implement them here
sources :: TestTypeSources
sources = TestTypeSources
  { _arrowTestCases = TestArrowSources
      { _simpleArrowTestCase = "-> Unit Unit"
      }

  , _bigArrowTestCases = TestBigArrowSources
      { _simpleBigArrowTestCase = "/-> KIND Unit"
      , _complexBigArrowTestCase = "/-> KIND ?0"
      }

  , _productTestCases = TestProductSources
      { _emptyProductTestCase = "(*)"
      , _singletonProductTestCase = "* Unit"
      , _twoProductTestCase = "* Unit Nat"
      , _duplicateProductTestCase = "* Unit Unit"
      }

  , _sumTestCases = TestSumSources
      { _sumTwoTestCase = "+ Unit Nat"
      , _singletonSumTestCase = "+ Unit"
      , _duplicateSumTestCase = "+ Unit Unit"
      }

  , _typeBindingTestCases = TestTypeBindingSources
     { _simpleTypeBindingTestCase = "?0"
     , _buriedTypeBindingTestCase =
         "ΛKIND (/@ (Λ(->KIND KIND) (ΛKIND ?1))\n\
         \          (ΛKIND ?1)\n\
         \      )"
     , _doubleBuriedTypeBindingTestCase =
         "ΛKIND (/@ (Λ(->KIND KIND) (ΛKIND (ΛKIND ?2)))\n\
         \          (ΛKIND ?1)\n\
         \      )"

     , _buriedTypeBindingsDontMoveTestCase = "ΛKIND (/@ (ΛKIND ?0) ?0)"
     }

  , _typeLamTestCases = TestTypeLamSources
      { _simpleTypeLamTestCase = "ΛKIND ?0"
      , _nestedTypeLamTestCase = "ΛKIND (ΛKIND ?1)"
      }

  , _unionTestCases = TestUnionSources
      { _unionTwoTestCase = "U Unit Nat"
      , _singletonUnionTestCase = "U Unit"
      , _duplicateUnionTestCase = "U Unit Unit"
      }

  , _namedTestCases = TestNamedSources
      { _simpleNameTestCase    = "Preexisting"
      , _recursiveNameTestCase = "Recursive"
      }
  }


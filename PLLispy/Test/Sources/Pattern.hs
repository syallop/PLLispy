{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLLispy.Test.Sources.Pattern
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Exports test source code fragments for the PL test cases.
This might be used for:
- This libraries test suite to ensure the grammar/ printers/ parsers are behaving
- External libraries who wish to do something with valid source code fragments,
  such as displaying examples.
-}
module PLLispy.Test.Sources.Pattern
  ( sources
  )
  where

import PL
import PL.Expr
import PL.Var
import PL.TyVar
import PL.Type
import PL.Test.Pattern
import PL.Test.PatternTestCase
import PL.Test.Pattern.Bind
import PL.Test.Pattern.Sum
import PL.Test.Pattern.Product
import PL.Test.Pattern.Union
import PL.Test.Pattern.Binding

import Data.Text
import qualified Data.Text as Text

sources :: TestPatternSources
sources = TestPatternSources
  { _bindTestCases    = TestBindSources
      { _bindEmptySum     = "?"
      , _bindEmptyProduct = "?"
      , _bindNamedBoolean = "?"
      }

  , _sumTestCases     = TestSumSources
      { _sumTestCase = "(+0 (*))"
      }

  , _productTestCases = TestProductSources
      { _productTestCase = "(*)"
      }

  , _unionTestCases   = TestUnionSources
      { _unionTestCase = "∪ (*) (*)"
      }

  , _bindingTestCases = TestBindingSources
      { _bindingTestCase = "0"
      }
  }


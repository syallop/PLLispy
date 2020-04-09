{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLLispy.Test.Sources.MatchArg
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Exports test source code fragments for the PL test cases.
This might be used for:
- This libraries test suite to ensure the grammar/ printers/ parsers are behaving
- External libraries who wish to do something with valid source code fragments,
  such as displaying examples.
-}
module PLLispy.Test.Sources.MatchArg
  ( sources
  )
  where

import PL
import PL.Expr
import PL.Var
import PL.TyVar
import PL.Type
import PL.Test.MatchArg
import PL.Test.MatchArgTestCase
import PL.Test.MatchArg.Bind
import PL.Test.MatchArg.Sum
import PL.Test.MatchArg.Product
import PL.Test.MatchArg.Union
import PL.Test.MatchArg.Binding

import Data.Text
import qualified Data.Text as Text

sources :: TestMatchArgSources
sources = TestMatchArgSources
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


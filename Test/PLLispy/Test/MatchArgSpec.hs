{-# LANGUAGE OverloadedStrings #-}
module PLLispy.Test.MatchArgSpec where

import PL
import PL.Test.MatchArg
import PL.Test.MatchArg.Bind
import PL.Test.MatchArg.Sum
import PL.Test.MatchArg.Product
import PL.Test.MatchArg.Union
import PL.Test.MatchArg.Binding

import PLLispy
import PLLispy.Expr
import PLLispy.Type
import PLLispy.MatchArg

import Data.Text

import Test.Hspec

spec
  :: Spec
spec = parserSpec lispySources lispyParser
  where
    lispyParser = toParser $ using var (typ tyVar) tyVar matchArg

    lispySources :: TestMatchArgSources
    lispySources = TestMatchArgSources
      { _bindTestCases    = TestBindSources
          { _bindEmptySum     = "?"
          , _bindEmptyProduct = "?"
          , _bindNamedBoolean = "?"
          }

      , _sumTestCases     = TestSumSources
          { _sumTestCase = "(+ (*))"
          }

      , _productTestCases = TestProductSources
          { _productTestCase = "(*)"
          }

      , _unionTestCases   = TestUnionSources
          { _unionTestCase = "∪ (*)"
          }

      , _bindingTestCases = TestBindingSources
          { _bindingTestCase = "0"
          }
      }


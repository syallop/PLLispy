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
import PL.Test.Type.Named
import PL.Test.TypeTestCase

import Data.Text
import qualified Data.Text as Text

-- TODO: Write some tests, implement them here
sources :: TestTypeSources
sources = TestTypeSources
  { _namedTestCases = TestNamedSources
      { _simpleNameTestCase = "Preexisting"
      }
  }


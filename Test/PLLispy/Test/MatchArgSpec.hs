{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  #-}
module PLLispy.Test.MatchArgSpec where

import PL
import PL.Var
import PL.Type
import PL.Expr
import PL.TyVar

import PL.Test.MatchArg
import PL.Test.MatchArgTestCase
import PL.Test.MatchArg.Bind
import PL.Test.MatchArg.Sum
import PL.Test.MatchArg.Product
import PL.Test.MatchArg.Union
import PL.Test.MatchArg.Binding

import PLLispy
import PLLispy.Test.Sources.MatchArg
import PLLispy.Expr
import PLLispy.Type
import PLLispy.MatchArg

import PLPrinter
import PLPrinter.Doc
import PLGrammar

import Data.Text
import Data.Maybe

import Test.Hspec

spec
  :: Spec
spec = parserSpec sources lispyParser ppType ppMatchArg
  where
    typeGrammar :: Grammar (Type TyVar)
    typeGrammar = typ tyVar

    ppType :: Type TyVar -> Doc
    ppType = fromMaybe mempty . pprint (toPrinter typeGrammar)

    matchArgGrammar :: Grammar TestMatchArg
    matchArgGrammar = using var (typ tyVar) tyVar matchArg

    ppMatchArg :: TestMatchArg -> Doc
    ppMatchArg = fromMaybe mempty . pprint (toPrinter matchArgGrammar)

    lispyParser = toParser matchArgGrammar


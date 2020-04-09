{-# LANGUAGE OverloadedStrings #-}
module PLLispy.Test.ExprSpec where

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

import PLLispy
import PLLispy.Expr
import PLLispy.Type
import PLLispy.Test.Sources.Expr

import PLGrammar
import PLPrinter
import PLPrinter.Doc

import Data.Text
import qualified Data.Text as Text
import Data.Monoid
import Data.Maybe

import Test.Hspec

spec
  :: Spec
spec = parserSpec sources lispyParser ppExpr ppType
  where
    typeGrammar :: Grammar (Type TyVar)
    typeGrammar = typ tyVar

    ppType :: Type TyVar -> Doc
    ppType = fromMaybe mempty . pprint (toPrinter typeGrammar)

    exprGrammar :: Grammar (Expr Var (Type TyVar) TyVar)
    exprGrammar = expr var typeGrammar tyVar

    ppExpr :: Expr Var (Type TyVar) TyVar -> Doc
    ppExpr = fromMaybe mempty . pprint (toPrinter exprGrammar)

    lispyParser = toParser exprGrammar


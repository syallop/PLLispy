{-# LANGUAGE OverloadedStrings #-}
module PLLispy.ExprIso where

import PLGrammar
import Reversible.Iso

import PLLispy.Kind
import PLLispy.Type

import Data.Text

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var
import PL.TyVar

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -
 - We _could_ make these more generic and use 'ExprFor' and the extension types.
 -}
lamIso :: Iso (Type, Expr) Expr
lamIso = Iso
  {_forwards = \(abs,body)
                -> Just $ Lam abs body
  ,_backwards = \expr
                -> case expr of
                     Lam abs body
                       -> Just (abs,body)
                     _ -> Nothing
  }

appIso :: Iso (Expr,Expr) Expr
appIso = Iso
  {_forwards = \(f,x)
                -> Just $ App f x
  ,_backwards = \expr
                -> case expr of
                     App f x
                       -> Just (f,x)
                     _ -> Nothing
  }

bindingIso :: Iso Var Expr
bindingIso = Iso
  {_forwards = \b
               -> Just . Binding $ b
  ,_backwards = \expr
               -> case expr of
                    Binding b
                      -> Just b
                    _ -> Nothing
  }

caseAnalysisIso :: Iso (Case Expr MatchArg) Expr
caseAnalysisIso = Iso
  {_forwards = \caseA
               -> Just . CaseAnalysis $ caseA
  ,_backwards = \expr
               -> case expr of
                    CaseAnalysis caseA
                      -> Just caseA
                    _ -> Nothing
  }

sumIso :: Iso (Int, (Expr, NonEmpty Type)) Expr
sumIso = Iso
  {_forwards = \(sumIx, (expr, inTypes))
               -> Just . Sum expr sumIx $ inTypes
  ,_backwards = \expr
               -> case expr of
                    Sum expr sumIx inTypes
                      -> Just (sumIx, (expr, inTypes))
                    _ -> Nothing
  }

productIso :: Iso [Expr] Expr
productIso = Iso
  {_forwards = \exprs
               -> Just . Product $ exprs
  ,_backwards = \expr
               -> case expr of
                    Product exprs
                      -> Just exprs
                    _ -> Nothing
  }

unionIso :: Iso (Type, (Expr, Set.Set Type)) Expr
unionIso = Iso
  {_forwards = \(unionIx, (expr, inTypes))
               -> Just . Union expr unionIx $ inTypes
  ,_backwards = \expr
               -> case expr of
                    Union expr unionIx inTypes
                      -> Just (unionIx, (expr, inTypes))
                    _ -> Nothing
  }

bigLamIso :: Iso (Kind, Expr) Expr
bigLamIso = Iso
  {_forwards = \(absKind, bodyExpr)
               -> Just . BigLam absKind $ bodyExpr
  ,_backwards = \expr
               -> case expr of
                    BigLam absKind bodyExpr
                      -> Just (absKind, bodyExpr)
                    _ -> Nothing
  }

bigAppIso :: Iso  (Expr, Type) Expr
bigAppIso = Iso
  {_forwards = \(f, xTy)
               -> Just . BigApp f $ xTy
  ,_backwards = \expr
               -> case expr of
                    BigApp f xTy
                      -> Just (f, xTy)
                    _ -> Nothing
  }


-- TODO: Doesnt belong here.
varIso :: Iso Int Var
varIso = Iso
  {_forwards = Just . mkVar
  ,_backwards = Just . fromEnum -- TODO: Partial
  }

-- TODO: Doesnt belong here.
setIso :: Ord a => Iso [a] (Set.Set a)
setIso = Iso
  {_forwards = Just . Set.fromList
  ,_backwards = Just . Set.toList
  }


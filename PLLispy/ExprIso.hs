{-# LANGUAGE OverloadedStrings #-}
module PLLispy.ExprIso where

import PLGrammar
import Reversible.Iso

import PLLispy.Kind
import PLLispy.Type

import Data.Text

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.FixExpr
import PL.Kind
import PL.Type
import PL.Var

import qualified Data.Set as Set

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}
lamIso :: Iso (abs,Expr b abs tb) (Expr b abs tb)
lamIso = Iso
  {_forwards = \(abs,body)
                -> Just $ fixExpr $ Lam abs body
  ,_backwards = \expr
                -> case unfixExpr expr of
                     Lam abs body
                       -> Just (abs,body)
                     _ -> Nothing
  }

appIso :: Iso (Expr b abs tb,Expr b abs tb) (Expr b abs tb)
appIso = Iso
  {_forwards = \(f,x)
                -> Just $ fixExpr $ App f x
  ,_backwards = \expr
                -> case unfixExpr expr of
                     App f x
                       -> Just (f,x)
                     _ -> Nothing
  }

bindingIso :: Iso b (Expr b abs tb)
bindingIso = Iso
  {_forwards = \b
               -> Just . fixExpr . Binding $ b
  ,_backwards = \expr
               -> case unfixExpr expr of
                    Binding b
                      -> Just b
                    _ -> Nothing
  }

caseAnalysisIso :: Iso (Case (Expr b abs tb) (MatchArg b tb)) (Expr b abs tb)
caseAnalysisIso = Iso
  {_forwards = \caseA
               -> Just . fixExpr . CaseAnalysis $ caseA
  ,_backwards = \expr
               -> case unfixExpr expr of
                    CaseAnalysis caseA
                      -> Just caseA
                    _ -> Nothing
  }

sumIso :: Iso (Int, (Expr b abs tb, [Type tb])) (Expr b abs tb)
sumIso = Iso
  {_forwards = \(sumIx, (expr, inTypes))
               -> Just . fixExpr . Sum expr sumIx $ inTypes
  ,_backwards = \expr
               -> case unfixExpr expr of
                    Sum expr sumIx inTypes
                      -> Just (sumIx, (expr, inTypes))
                    _ -> Nothing
  }

productIso :: Iso [Expr b abs tb] (Expr b abs tb)
productIso = Iso
  {_forwards = \exprs
               -> Just . fixExpr . Product $ exprs
  ,_backwards = \expr
               -> case unfixExpr expr of
                    Product exprs
                      -> Just exprs
                    _ -> Nothing
  }

unionIso :: Iso (Type tb, (Expr b abs tb, Set.Set (Type tb))) (Expr b abs tb)
unionIso = Iso
  {_forwards = \(unionIx, (expr, inTypes))
               -> Just . fixExpr . Union expr unionIx $ inTypes
  ,_backwards = \expr
               -> case unfixExpr expr of
                    Union expr unionIx inTypes
                      -> Just (unionIx, (expr, inTypes))
                    _ -> Nothing
  }

bigLamIso :: Iso (Kind, Expr b abs tb) (Expr b abs tb)
bigLamIso = Iso
  {_forwards = \(absKind, bodyExpr)
               -> Just . fixExpr . BigLam absKind $ bodyExpr
  ,_backwards = \expr
               -> case unfixExpr expr of
                    BigLam absKind bodyExpr
                      -> Just (absKind, bodyExpr)
                    _ -> Nothing
  }

bigAppIso :: Iso  (Expr b abs tb, Type tb) (Expr b abs tb)
bigAppIso = Iso
  {_forwards = \(f, xTy)
               -> Just . fixExpr . BigApp f $ xTy
  ,_backwards = \expr
               -> case unfixExpr expr of
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


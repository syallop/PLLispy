{-# LANGUAGE OverloadedStrings, GADTs #-}
module PLLispy.Expr.Iso
  ( lamIso
  , appIso
  , bindingIso
  , contentBindingIso
  , caseAnalysisIso
  , sumIso
  , productIso
  , unionIso
  , bigLamIso
  , bigAppIso
  , exprExtensionIso

  , commentedExprIso

  , setIso
  )
  where

import PLGrammar
import Reversible.Iso

import PLLispy.Kind
import PLLispy.Type

import Data.Text

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Name
import PL.Pattern
import PL.TyVar
import PL.Type
import PL.Var

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}
lamIso :: Iso (LamExtension phase, (AbstractionFor phase, ExprFor phase)) (ExprFor phase)
lamIso = Iso
  {_forwards = \(ext,(abs,body))
                -> Just $ LamExt ext abs body
  ,_backwards = \expr
                -> case expr of
                     LamExt ext abs body
                       -> Just (ext,(abs,body))
                     _ -> Nothing
  }

appIso :: Iso (AppExtension phase, (ExprFor phase, ExprFor phase)) (ExprFor phase)
appIso = Iso
  {_forwards = \(ext,(f,x))
                -> Just $ AppExt ext f x
  ,_backwards = \expr
                -> case expr of
                     AppExt ext f x
                       -> Just (ext,(f,x))
                     _ -> Nothing
  }

bindingIso :: Iso (BindingExtension phase, BindingFor phase) (ExprFor phase)
bindingIso = Iso
  {_forwards = \(ext,b)
               -> Just . BindingExt ext $ b
  ,_backwards = \expr
               -> case expr of
                    BindingExt ext b
                      -> Just (ext, b)
                    _ -> Nothing
  }

contentBindingIso :: Iso (ContentBindingExtension phase, ContentBindingFor phase) (ExprFor phase)
contentBindingIso = Iso
  {_forwards = \(ext,c)
                -> Just . ContentBindingExt ext $ c
  ,_backwards = \expr
                 -> case expr of
                      ContentBindingExt ext c
                        -> Just (ext, c)
                      _ -> Nothing
  }

caseAnalysisIso :: Iso (CaseAnalysisExtension phase, Case (ExprFor phase) (PatternFor phase)) (ExprFor phase)
caseAnalysisIso = Iso
  {_forwards = \(ext, caseA)
               -> Just . CaseAnalysisExt ext $ caseA
  ,_backwards = \expr
               -> case expr of
                    CaseAnalysisExt ext caseA
                      -> Just (ext, caseA)
                    _ -> Nothing
  }

sumIso :: Iso (SumExtension phase, (Int, (ExprFor phase, NonEmpty (TypeFor phase)))) (ExprFor phase)
sumIso = Iso
  {_forwards = \(ext, (sumIx, (expr, inTypes)))
               -> Just . SumExt ext expr sumIx $ inTypes
  ,_backwards = \expr
               -> case expr of
                    SumExt ext expr sumIx inTypes
                      -> Just (ext, (sumIx, (expr, inTypes)))
                    _ -> Nothing
  }

productIso :: Iso (ProductExtension phase, [ExprFor phase]) (ExprFor phase)
productIso = Iso
  {_forwards = \(ext, exprs)
               -> Just . ProductExt ext $ exprs
  ,_backwards = \expr
               -> case expr of
                    ProductExt ext exprs
                      -> Just (ext, exprs)
                    _ -> Nothing
  }

unionIso :: Iso (UnionExtension phase, (TypeFor phase, (ExprFor phase, Set.Set (TypeFor phase)))) (ExprFor phase)
unionIso = Iso
  {_forwards = \(ext, (unionIx, (expr, inTypes)))
               -> Just . UnionExt ext expr unionIx $ inTypes
  ,_backwards = \expr
               -> case expr of
                    UnionExt ext expr unionIx inTypes
                      -> Just (ext, (unionIx, (expr, inTypes)))
                    _ -> Nothing
  }

bigLamIso :: Iso (BigLamExtension phase, (Kind, ExprFor phase)) (ExprFor phase)
bigLamIso = Iso
  {_forwards = \(ext, (absKind, bodyExpr))
               -> Just . BigLamExt ext absKind $ bodyExpr
  ,_backwards = \expr
               -> case expr of
                    BigLamExt ext absKind bodyExpr
                      -> Just (ext, (absKind, bodyExpr))
                    _ -> Nothing
  }

bigAppIso :: Iso (BigAppExtension phase, (ExprFor phase, TypeFor phase)) (ExprFor phase)
bigAppIso = Iso
  {_forwards = \(ext, (f, xTy))
               -> Just . BigAppExt ext f $ xTy
  ,_backwards = \expr
               -> case expr of
                    BigAppExt ext f xTy
                      -> Just (ext, (f, xTy))
                    _ -> Nothing
  }

exprExtensionIso :: Iso (ExprExtension phase) (ExprFor phase)
exprExtensionIso = Iso
  {_forwards = \ext
                -> Just . ExprExtensionExt $ ext
  ,_backwards = \expr
                 -> case expr of
                      ExprExtensionExt ext
                        -> Just ext
                      _ -> Nothing
  }



-- TODO: Below don't particularly belong here

commentedExprIso :: Iso (Comment, ExprFor phase) (Commented (ExprFor phase))
commentedExprIso = Iso
  {_forwards = \(c,expr)
                -> Just . Commented c $ expr
  ,_backwards = \(Commented c expr) -> Just (c, expr)
  }

setIso :: Ord a => Iso [a] (Set.Set a)
setIso = Iso
  {_forwards = Just . Set.fromList
  ,_backwards = Just . Set.toList
  }


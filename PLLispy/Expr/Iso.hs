{-# LANGUAGE OverloadedStrings, GADTs, LambdaCase #-}
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

import Reversible.Iso

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Pattern
import PL.Type

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}
lamIso :: Iso (LamExtension phase, (AbstractionFor phase, ExprFor phase)) (ExprFor phase)
lamIso = Iso
  {_forwards = \(ext,(abstract,body))
                -> Just $ LamExt ext abstract body
  ,_backwards = \case
                  LamExt ext abstract body
                    -> Just (ext,(abstract,body))
                  _ -> Nothing
  }

appIso :: Iso (AppExtension phase, (ExprFor phase, ExprFor phase)) (ExprFor phase)
appIso = Iso
  {_forwards = \(ext,(f,x))
                -> Just $ AppExt ext f x
  ,_backwards = \case
                   AppExt ext f x
                     -> Just (ext,(f,x))
                   _ -> Nothing
  }

bindingIso :: Iso (BindingExtension phase, BindingFor phase) (ExprFor phase)
bindingIso = Iso
  {_forwards = \(ext,b)
               -> Just . BindingExt ext $ b
  ,_backwards = \case
                  BindingExt ext b
                    -> Just (ext, b)
                  _ -> Nothing
  }

contentBindingIso :: Iso (ContentBindingExtension phase, ContentBindingFor phase) (ExprFor phase)
contentBindingIso = Iso
  {_forwards = \(ext,c)
                -> Just . ContentBindingExt ext $ c
  ,_backwards = \case
                  ContentBindingExt ext c
                    -> Just (ext, c)
                  _ -> Nothing
  }

caseAnalysisIso :: Iso (CaseAnalysisExtension phase, Case (ExprFor phase) (PatternFor phase)) (ExprFor phase)
caseAnalysisIso = Iso
  {_forwards = \(ext, caseA)
               -> Just . CaseAnalysisExt ext $ caseA
  ,_backwards = \case
                  CaseAnalysisExt ext caseA
                    -> Just (ext, caseA)
                  _ -> Nothing
  }

sumIso :: Iso (SumExtension phase, (Int, (ExprFor phase, NonEmpty (TypeFor phase)))) (ExprFor phase)
sumIso = Iso
  {_forwards = \(ext, (sumIx, (expr, inTypes)))
               -> Just . SumExt ext expr sumIx $ inTypes
  ,_backwards = \case
                  SumExt ext expr sumIx inTypes
                    -> Just (ext, (sumIx, (expr, inTypes)))
                  _ -> Nothing
  }

productIso :: Iso (ProductExtension phase, [ExprFor phase]) (ExprFor phase)
productIso = Iso
  {_forwards = \(ext, exprs)
               -> Just . ProductExt ext $ exprs
  ,_backwards = \case
                  ProductExt ext exprs
                    -> Just (ext, exprs)
                  _ -> Nothing
  }

unionIso :: Iso (UnionExtension phase, (TypeFor phase, (ExprFor phase, Set.Set (TypeFor phase)))) (ExprFor phase)
unionIso = Iso
  {_forwards = \(ext, (unionIx, (expr, inTypes)))
               -> Just . UnionExt ext expr unionIx $ inTypes
  ,_backwards = \case
                  UnionExt ext expr unionIx inTypes
                    -> Just (ext, (unionIx, (expr, inTypes)))
                  _ -> Nothing
  }

bigLamIso :: Iso (BigLamExtension phase, (Kind, ExprFor phase)) (ExprFor phase)
bigLamIso = Iso
  {_forwards = \(ext, (abstractKind, bodyExpr))
               -> Just . BigLamExt ext abstractKind $ bodyExpr
  ,_backwards = \case
                  BigLamExt ext abstractKind bodyExpr
                    -> Just (ext, (abstractKind, bodyExpr))
                  _ -> Nothing
  }

bigAppIso :: Iso (BigAppExtension phase, (ExprFor phase, TypeFor phase)) (ExprFor phase)
bigAppIso = Iso
  {_forwards = \(ext, (f, xTy))
               -> Just . BigAppExt ext f $ xTy
  ,_backwards = \case
                  BigAppExt ext f xTy
                    -> Just (ext, (f, xTy))
                  _ -> Nothing
  }

exprExtensionIso :: Iso (ExprExtension phase) (ExprFor phase)
exprExtensionIso = Iso
  {_forwards = \ext
                -> Just . ExprExtensionExt $ ext
  ,_backwards = \case
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


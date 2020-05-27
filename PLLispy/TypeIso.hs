{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PLLispy.TypeIso where

import PLGrammar
import Reversible.Iso

import PLLispy.Kind

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.TyVar
import PL.Commented
import PL.Name
import PL.Var

import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty)

import Data.Char

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

-- TODO: Doesnt belong here
typeNameIso :: Iso Text.Text TypeName
typeNameIso = Iso
  {_forwards = mkTypeName
  ,_backwards = Just . typeName
  }

namedIso :: Iso TypeName CommentedType
namedIso = Iso
  {_forwards = \typeName
                -> Just . Named $ typeName
  ,_backwards = \ty
                -> case ty of
                     Named typeName
                       -> Just typeName
                     _ -> Nothing
  }

arrowIso :: Iso (CommentedType, CommentedType) CommentedType
arrowIso = Iso
  {_forwards = \(fromTy, toTy)
                -> Just . Arrow fromTy $ toTy
  ,_backwards = \ty
                -> case ty of
                     Arrow fromTy toTy
                       -> Just (fromTy, toTy)
                     _ -> Nothing
  }

sumTIso :: Iso (NonEmpty CommentedType) CommentedType
sumTIso = Iso
  {_forwards = \tys
                -> Just . SumT $ tys
  ,_backwards = \ty
                -> case ty of
                     SumT tys
                       -> Just tys
                     _ -> Nothing
  }

productTIso :: Iso [CommentedType] CommentedType
productTIso = Iso
  {_forwards = \tys
                -> Just . ProductT $ tys
  ,_backwards = \ty
                -> case ty of
                     ProductT tys
                       -> Just tys
                     _ -> Nothing
  }

unionTIso :: Iso (Set.Set CommentedType) CommentedType
unionTIso = Iso
  {_forwards = \tys
                -> Just . UnionT $ tys
  ,_backwards = \ty
                -> case ty of
                     UnionT tys
                       -> Just tys
                     _ -> Nothing
  }

bigArrowIso :: Iso (Kind, CommentedType) CommentedType
bigArrowIso = Iso
  {_forwards = \(fromKind, toTy)
                -> Just . BigArrow fromKind $ toTy
  ,_backwards = \ty
                -> case ty of
                    BigArrow fromKind toTy
                      -> Just (fromKind, toTy)
                    _ -> Nothing
  }

typeLamIso :: Iso (Kind, CommentedType) CommentedType
typeLamIso = Iso
  {_forwards = \(fromKind, toTy)
                -> Just . TypeLam fromKind $ toTy
  ,_backwards = \ty
                -> case ty of
                    TypeLam fromKind toTy
                      -> Just (fromKind, toTy)
                    _ -> Nothing
  }


typeAppIso :: Iso (CommentedType, CommentedType) CommentedType
typeAppIso = Iso
  {_forwards = \(fTy, xTy)
                -> Just . TypeApp fTy $ xTy
  ,_backwards = \ty
                -> case ty of
                     TypeApp fTy xTy
                       -> Just (fTy, xTy)
                     _ -> Nothing
  }

typeBindingIso :: Iso TyVar CommentedType
typeBindingIso = Iso
  {_forwards = \tb
                -> Just . TypeBinding $ tb
  ,_backwards = \ty
                -> case ty of
                     TypeBinding tb
                       -> Just tb
                     _ -> Nothing
  }

typeContentBindingIso :: Iso ContentName CommentedType
typeContentBindingIso = Iso
  {_forwards = \c
                -> Just . TypeContentBinding $ c
  ,_backwards = \ty
                 -> case ty of
                      TypeContentBinding c
                        -> Just c
                      _ -> Nothing
  }

commentedIso :: Iso (Comment, CommentedType) CommentedType
commentedIso = Iso
  {_forwards = \(c,typ)
                -> Just . CommentedType c $ typ
  ,_backwards = \typ
                 -> case typ of
                      CommentedType c typ
                        -> Just (c, typ)
                      _ -> Nothing
  }

-- TODO: Doesnt belong here.
-- TODO: Partial.
tyVarIso :: Iso Int TyVar
tyVarIso = Iso
  {_forwards = Just . mkTyVar
  ,_backwards = \(TyVar v) -> Just . fromEnum $ v
  }

setIso :: Ord a => Iso [a] (Set.Set a)
setIso = Iso
  {_forwards = Just . Set.fromList
  ,_backwards = Just . Set.toList
  }


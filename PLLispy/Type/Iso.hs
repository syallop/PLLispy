{-# LANGUAGE
    MultiWayIf
  , OverloadedStrings
  , GADTs
  #-}
module PLLispy.Type.Iso
  ( typeNameIso
  , namedIso
  , arrowIso
  , sumTIso
  , productTIso
  , unionTIso
  , bigArrowIso
  , typeLamIso
  , typeAppIso
  , typeBindingIso
  , typeContentBindingIso
  , typeExtensionIso

  , commentedTypeIso

  , tyVarIso
  , setIso
  )
  where

import Data.Char
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import qualified Data.Text as Text

import PLLispy.Kind

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Name
import PL.TyVar
import PL.Type
import PL.Var

import PLGrammar
import Reversible.Iso


{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

-- TODO: Doesnt belong here
typeNameIso :: Iso Text.Text TypeName
typeNameIso = Iso
  {_forwards  = mkTypeName
  ,_backwards = Just . typeName
  }

namedIso :: Iso (NamedExtension phase, TypeName) (TypeFor phase)
namedIso = Iso
  {_forwards = \(ext,typeName)
                -> Just . NamedExt ext $ typeName
  ,_backwards = \ty
                -> case ty of
                     NamedExt ext typeName
                       -> Just (ext,typeName)
                     _ -> Nothing
  }

arrowIso :: Iso (ArrowExtension phase, (TypeFor phase, TypeFor phase)) (TypeFor phase)
arrowIso = Iso
  {_forwards = \(ext, (fromTy, toTy))
                -> Just . ArrowExt ext fromTy $ toTy
  ,_backwards = \ty
                -> case ty of
                     ArrowExt ext fromTy toTy
                       -> Just (ext, (fromTy, toTy))
                     _ -> Nothing
  }

sumTIso :: Iso (SumTExtension phase, NonEmpty (TypeFor phase)) (TypeFor phase)
sumTIso = Iso
  {_forwards = \(ext,tys)
                -> Just . SumTExt ext $ tys
  ,_backwards = \ty
                -> case ty of
                     SumTExt ext tys
                       -> Just (ext,tys)
                     _ -> Nothing
  }

productTIso :: Iso (ProductTExtension phase, [TypeFor phase]) (TypeFor phase)
productTIso = Iso
  {_forwards = \(ext,tys)
                -> Just . ProductTExt ext $ tys
  ,_backwards = \ty
                -> case ty of
                     ProductTExt ext tys
                       -> Just (ext,tys)
                     _ -> Nothing
  }

unionTIso :: Iso (UnionTExtension phase, Set.Set (TypeFor phase)) (TypeFor phase)
unionTIso = Iso
  {_forwards = \(ext,tys)
                -> Just . UnionTExt ext $ tys
  ,_backwards = \ty
                -> case ty of
                     UnionTExt ext tys
                       -> Just (ext,tys)
                     _ -> Nothing
  }

bigArrowIso :: Iso (BigArrowExtension phase, (Kind, TypeFor phase)) (TypeFor phase)
bigArrowIso = Iso
  {_forwards = \(ext, (fromKind, toTy))
                -> Just . BigArrowExt ext fromKind $ toTy
  ,_backwards = \ty
                -> case ty of
                    BigArrowExt ext fromKind toTy
                      -> Just (ext, (fromKind, toTy))
                    _ -> Nothing
  }

typeLamIso :: Iso (TypeLamExtension phase, (Kind, TypeFor phase)) (TypeFor phase)
typeLamIso = Iso
  {_forwards = \(ext, (fromKind, toTy))
                -> Just . TypeLamExt ext fromKind $ toTy
  ,_backwards = \ty
                -> case ty of
                    TypeLamExt ext fromKind toTy
                      -> Just (ext, (fromKind, toTy))
                    _ -> Nothing
  }


typeAppIso :: Iso (TypeAppExtension phase, (TypeFor phase, TypeFor phase)) (TypeFor phase)
typeAppIso = Iso
  {_forwards = \(ext, (fTy, xTy))
                -> Just . TypeAppExt ext fTy $ xTy
  ,_backwards = \ty
                -> case ty of
                     TypeAppExt ext fTy xTy
                       -> Just (ext, (fTy, xTy))
                     _ -> Nothing
  }

typeBindingIso :: Iso (TypeBindingExtension phase, TypeBindingFor phase) (TypeFor phase)
typeBindingIso = Iso
  {_forwards = \(ext, tb)
                -> Just . TypeBindingExt ext $ tb
  ,_backwards = \ty
                -> case ty of
                     TypeBindingExt ext tb
                       -> Just (ext, tb)
                     _ -> Nothing
  }

typeContentBindingIso :: Iso (TypeContentBindingExtension phase, TypeContentBindingFor phase) (TypeFor phase)
typeContentBindingIso = Iso
  {_forwards = \(ext, c)
                -> Just . TypeContentBindingExt ext $ c
  ,_backwards = \ty
                 -> case ty of
                      TypeContentBindingExt ext c
                        -> Just (ext, c)
                      _ -> Nothing
  }

typeExtensionIso :: Iso (TypeExtension phase) (TypeFor phase)
typeExtensionIso = Iso
  {_forwards = \ext
                -> Just . TypeExtensionExt $ ext
  ,_backwards = \ty
                 -> case ty of
                      TypeExtensionExt ext
                        -> Just ext
                      _ -> Nothing
  }

commentedTypeIso :: Iso (Comment, TypeFor phase) (Commented (TypeFor phase))
commentedTypeIso = Iso
  {_forwards = \(c,ty)
                -> Just . Commented c $ ty
  ,_backwards = \(Commented c ty) -> Just (c, ty)
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


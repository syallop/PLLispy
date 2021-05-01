{-# LANGUAGE
    MultiWayIf
  , OverloadedStrings
  , GADTs
  , LambdaCase
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
  , typeMuIso
  , typeBindingIso
  , typeSelfBindingIso
  , typeContentBindingIso
  , typeExtensionIso

  , commentedTypeIso

  , tyVarIso
  , setIso
  )
  where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import qualified Data.Text as Text

import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Name
import PL.TyVar
import PL.Type
import PL.FixPhase

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
  {_forwards = \(ext,name)
                -> Just . NamedExt ext $ name
  ,_backwards = \case
                  NamedExt ext name
                    -> Just (ext,name)
                  _ -> Nothing
  }

arrowIso :: Iso (ArrowExtension phase, (TypeFor phase, TypeFor phase)) (TypeFor phase)
arrowIso = Iso
  {_forwards = \(ext, (fromTy, toTy))
                -> Just . ArrowExt ext fromTy $ toTy
  ,_backwards = \case
                  ArrowExt ext fromTy toTy
                    -> Just (ext, (fromTy, toTy))
                  _ -> Nothing
  }

sumTIso :: Iso (SumTExtension phase, NonEmpty (TypeFor phase)) (TypeFor phase)
sumTIso = Iso
  {_forwards = \(ext,tys)
                -> Just . SumTExt ext $ tys
  ,_backwards = \case
                  SumTExt ext tys
                    -> Just (ext,tys)
                  _ -> Nothing
  }

productTIso :: Iso (ProductTExtension phase, [TypeFor phase]) (TypeFor phase)
productTIso = Iso
  {_forwards = \(ext,tys)
                -> Just . ProductTExt ext $ tys
  ,_backwards = \case
                  ProductTExt ext tys
                    -> Just (ext,tys)
                  _ -> Nothing
  }

unionTIso :: Iso (UnionTExtension phase, Set.Set (TypeFor phase)) (TypeFor phase)
unionTIso = Iso
  {_forwards = \(ext,tys)
                -> Just . UnionTExt ext $ tys
  ,_backwards = \case
                  UnionTExt ext tys
                    -> Just (ext,tys)
                  _ -> Nothing
  }

bigArrowIso :: Iso (BigArrowExtension phase, (Kind, TypeFor phase)) (TypeFor phase)
bigArrowIso = Iso
  {_forwards = \(ext, (fromKind, toTy))
                -> Just . BigArrowExt ext fromKind $ toTy
  ,_backwards = \case
                  BigArrowExt ext fromKind toTy
                    -> Just (ext, (fromKind, toTy))
                  _ -> Nothing
  }

typeLamIso :: Iso (TypeLamExtension phase, (Kind, TypeFor phase)) (TypeFor phase)
typeLamIso = Iso
  {_forwards = \(ext, (fromKind, toTy))
                -> Just . TypeLamExt ext fromKind $ toTy
  ,_backwards = \case
                  TypeLamExt ext fromKind toTy
                    -> Just (ext, (fromKind, toTy))
                  _ -> Nothing
  }


typeAppIso :: Iso (TypeAppExtension phase, (TypeFor phase, TypeFor phase)) (TypeFor phase)
typeAppIso = Iso
  {_forwards = \(ext, (fTy, xTy))
                -> Just . TypeAppExt ext fTy $ xTy
  ,_backwards = \case
                  TypeAppExt ext fTy xTy
                    -> Just (ext, (fTy, xTy))
                  _ -> Nothing
  }

typeMuIso :: Iso (NoExt, (Kind, TypeFor phase)) (TypeFor phase)
typeMuIso = Iso
  {_forwards = \(ext, (expectKind, itselfTy))
                -> Just . TypeMuExt ext expectKind $ itselfTy
  ,_backwards = \case
                  TypeMuExt ext expectKind itselfTy
                    -> Just (ext, (expectKind, itselfTy))
                  _ -> Nothing
  }

typeBindingIso :: Iso (TypeBindingExtension phase, TypeBindingFor phase) (TypeFor phase)
typeBindingIso = Iso
  {_forwards = \(ext, tb)
                -> Just . TypeBindingExt ext $ tb
  ,_backwards = \case
                  TypeBindingExt ext tb
                    -> Just (ext, tb)
                  _ -> Nothing
  }

typeSelfBindingIso :: Iso NoExt (TypeFor phase)
typeSelfBindingIso = Iso
  {_forwards = \ext
                -> Just . TypeSelfBindingExt $ ext
  ,_backwards = \case
                  TypeSelfBindingExt ext
                    -> Just ext
                  _ -> Nothing
  }


typeContentBindingIso :: Iso (TypeContentBindingExtension phase, TypeContentBindingFor phase) (TypeFor phase)
typeContentBindingIso = Iso
  {_forwards = \(ext, c)
                -> Just . TypeContentBindingExt ext $ c
  ,_backwards = \case
                  TypeContentBindingExt ext c
                    -> Just (ext, c)
                  _ -> Nothing
  }

typeExtensionIso :: Iso (TypeExtension phase) (TypeFor phase)
typeExtensionIso = Iso
  {_forwards = \ext
                -> Just . TypeExtensionExt $ ext
  ,_backwards = \case
                  TypeExtensionExt ext
                    -> Just ext
                  _ -> Nothing
  }

commentedTypeIso :: Iso (Comment, TypeFor phase) (Commented (TypeFor phase))
commentedTypeIso = Iso
  {_forwards = \(c,t)
                -> Just . Commented c $ t
  ,_backwards = \(Commented c t) -> Just (c, t)
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


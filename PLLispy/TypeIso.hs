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
import PL.Name
import PL.Var
import PL.FixType

import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.List.NonEmpty (NonEmpty)

import Data.Char

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

-- A Name is text with an upper case first character and lowercase remaining
-- letters.
-- TODO: Doesnt belong here.
nameIso :: Iso (Char, Text.Text) Text.Text
nameIso = Iso
  {_forwards = \(c,cs) -> if
               | isUpper c && Text.all isLower cs -> Just $ Text.cons c cs
               | otherwise -> Nothing
  ,_backwards = \txt -> case Text.uncons txt of
                         Nothing
                           -> Nothing

                         Just (c,cs)
                           | isUpper c && Text.all isLower cs -> Just (c,cs)
                           | otherwise -> Nothing
  }

-- TODO: Doesnt belong here
typeNameIso :: Iso Text.Text TypeName
typeNameIso = Iso
  {_forwards = \txt -> Just $ TypeName txt
  ,_backwards = \(TypeName txt) -> Just txt
  }

namedIso :: Iso TypeName (Type tb)
namedIso = Iso
  {_forwards = \typeName
                -> Just . fixType . Named $ typeName
  ,_backwards = \ty
                -> case unfixType ty of
                     Named typeName
                       -> Just typeName
                     _ -> Nothing
  }

arrowIso :: Iso (Type tb, Type tb) (Type tb)
arrowIso = Iso
  {_forwards = \(fromTy, toTy)
                -> Just .fixType . Arrow fromTy $ toTy
  ,_backwards = \ty
                -> case unfixType ty of
                     Arrow fromTy toTy
                       -> Just (fromTy, toTy)
                     _ -> Nothing
  }

sumTIso :: Iso (NonEmpty (Type tb)) (Type tb)
sumTIso = Iso
  {_forwards = \tys
                -> Just . fixType . SumT $ tys
  ,_backwards = \ty
                -> case unfixType ty of
                     SumT tys
                       -> Just tys
                     _ -> Nothing
  }

productTIso :: Iso [Type tb] (Type tb)
productTIso = Iso
  {_forwards = \tys
                -> Just . fixType . ProductT $ tys
  ,_backwards = \ty
                -> case unfixType ty of
                     ProductT tys
                       -> Just tys
                     _ -> Nothing
  }

unionTIso :: Iso (Set.Set (Type tb)) (Type tb)
unionTIso = Iso
  {_forwards = \tys
                -> Just . fixType . UnionT $ tys
  ,_backwards = \ty
                -> case unfixType ty of
                     UnionT tys
                       -> Just tys
                     _ -> Nothing
  }

bigArrowIso :: Iso (Kind, Type tb) (Type tb)
bigArrowIso = Iso
  {_forwards = \(fromKind, toTy)
                -> Just . fixType . BigArrow fromKind $ toTy
  ,_backwards = \ty
                -> case unfixType ty of
                    BigArrow fromKind toTy
                      -> Just (fromKind, toTy)
                    _ -> Nothing
  }

typeLamIso :: Iso (Kind, Type tb) (Type tb)
typeLamIso = Iso
  {_forwards = \(fromKind, toTy)
                -> Just . fixType . TypeLam fromKind $ toTy
  ,_backwards = \ty
                -> case unfixType ty of
                    TypeLam fromKind toTy
                      -> Just (fromKind, toTy)
                    _ -> Nothing
  }


typeAppIso :: Iso (Type tb, Type tb) (Type tb)
typeAppIso = Iso
  {_forwards = \(fTy, xTy)
                -> Just . fixType . TypeApp fTy $ xTy
  ,_backwards = \ty
                -> case unfixType ty of
                     TypeApp fTy xTy
                       -> Just (fTy, xTy)
                     _ -> Nothing
  }

typeBindingIso :: Iso tb (Type tb)
typeBindingIso = Iso
  {_forwards = \tb
                -> Just . fixType . TypeBinding $ tb
  ,_backwards = \ty
                -> case unfixType ty of
                     TypeBinding tb
                       -> Just tb
                     _ -> Nothing
  }

-- TODO: Doesnt belong here.
-- TODO: Partial.
tyVarIso :: Iso Int TyVar
tyVarIso = Iso
  {_forwards = Just . mkTyVar
  ,_backwards = \(TyVar v) -> Just . fromEnum $ v
  }

-- TODO: Doesnt belong here.
setIso :: Ord a => Iso [a] (Set.Set a)
setIso = Iso
  {_forwards = Just . Set.fromList
  ,_backwards = Just . Set.toList
  }


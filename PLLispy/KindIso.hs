{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PLLispy.KindIso where

import PLGrammar
import Reversible.Iso

import PL.Case
import PL.Kind
import PL.TyVar
import PL.Name
import PL.Var

import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Char

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

kindIso :: Iso () Kind
kindIso = Iso
  {_forwards = \() -> Just Kind
  ,_backwards = \Kind -> Just ()
  }

kindArrowIso :: Iso (Kind,Kind) Kind
kindArrowIso = Iso
  {_forwards = \(fromKy, toKy) -> Just $ KindArrow fromKy toKy
  ,_backwards = \kind -> case kind of
                          KindArrow fromKy toKy
                            -> Just (fromKy, toKy)

                          _ -> Nothing
  }


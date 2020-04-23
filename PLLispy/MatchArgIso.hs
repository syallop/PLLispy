{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PLLispy.MatchArgIso where

import PLGrammar
import Reversible.Iso

import PL.Case
import PL.Expr
import PL.Type
import PL.Kind
import PL.TyVar
import PL.Name
import PL.Var

import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Char
import Data.List.NonEmpty

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -
 - We _could_ make these more generic and use 'MatchArgFor' and the extension
 - types.
 -}

matchSumIso :: Iso (Int, MatchArg) MatchArg
matchSumIso = Iso
  {_forwards = \(ix, matchArg) -> Just $ MatchSum ix matchArg
  ,_backwards = \matchArg -> case matchArg of
                              MatchSum ix matchArg
                                -> Just (ix, matchArg)
                              _ -> Nothing
  }

matchProductIso :: Iso [MatchArg] MatchArg
matchProductIso = Iso
  {_forwards = \matchArgs -> Just $ MatchProduct matchArgs
  ,_backwards = \matchArg -> case matchArg of
                              MatchProduct matchArgs
                                -> Just matchArgs
                              _ -> Nothing
  }

matchUnionIso :: Iso (Type, MatchArg) MatchArg
matchUnionIso = Iso
  {_forwards = \(tyIx, matchArg) -> Just $ MatchUnion tyIx matchArg
  ,_backwards = \matchArg -> case matchArg of
                              MatchUnion tyIx matchArg
                                -> Just (tyIx, matchArg)
                              _ -> Nothing
  }

matchBindingIso :: Iso Var MatchArg
matchBindingIso = Iso
  {_forwards = \b -> Just $ MatchBinding b
  ,_backwards = \matchArg -> case matchArg of
                              MatchBinding b
                                -> Just b
                              _ -> Nothing
  }

matchBindIso :: Iso () MatchArg
matchBindIso = Iso
  {_forwards = \() -> Just Bind
  ,_backwards = \matchArg -> case matchArg of
                              Bind
                                -> Just ()
                              _ -> Nothing
  }


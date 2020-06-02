{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PLLispy.Case.Iso
  ( caseIso
  , caseBranchesIso
  , defaultOnlyIso
  , caseBranchIso
  )
  where

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
import Data.List.NonEmpty

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

caseIso :: Iso (e, CaseBranches e m) (Case e m)
caseIso = Iso
  {_forwards = \(scrutineeExpr, branches) -> Just $ Case scrutineeExpr branches
  ,_backwards = \(Case scrutineeExpr branches) -> Just (scrutineeExpr, branches)
  }

caseBranchesIso :: Iso (NonEmpty (CaseBranch e m), Maybe e) (CaseBranches e m)
caseBranchesIso = Iso
  {_forwards = \(branches, mDefaultBranch) -> Just $ CaseBranches branches mDefaultBranch
  ,_backwards = \caseBranches -> case caseBranches of
                                  CaseBranches branches mDefaultBranch
                                    -> Just (branches, mDefaultBranch)
                                  _ -> Nothing
  }

defaultOnlyIso :: Iso e (CaseBranches e m)
defaultOnlyIso = Iso
  {_forwards = \resultExpr -> Just $ DefaultOnly resultExpr
  ,_backwards = \caseBranches -> case caseBranches of
                                  DefaultOnly resultExpr
                                    -> Just resultExpr
                                  _ -> Nothing
  }

caseBranchIso :: Iso (m, e) (CaseBranch e m)
caseBranchIso = Iso
  {_forwards = \(match, result) -> Just $ CaseBranch match result
  ,_backwards = \(CaseBranch match result) -> Just (match, result)
  }

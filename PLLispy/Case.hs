{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PLLispy.Case
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr.Case with a lisp-like syntax.
-}
module PLLispy.Case where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.List.NonEmpty

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.MatchArg
import PLLispy.CaseIso
import PLLispy.Kind
import PLLispy.Type
import PLLispy.Level

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var
import PL.TyVar

-- A Case body is made up of:
-- - A Scrutinee expression
-- - Either:
--   - A default expression
--   - One or many branches and an optional default expression
caseBody
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedExpr
  -> Grammar (Case CommentedExpr CommentedMatchArg)
caseBody expr =
  caseAnalysisIso \$/ expr
                  \*/ alternatives [ try defaultOnly
                                   , branchesAndOptionalDefault
                                   ]
  where
    defaultOnly :: Grammar (CaseBranches CommentedExpr CommentedMatchArg)
    defaultOnly = defaultOnlyIso \$/ (spacePreferred */ expr)

    branchesAndOptionalDefault :: Grammar (CaseBranches CommentedExpr CommentedMatchArg)
    branchesAndOptionalDefault =
      branchesAndOptionalDefaultIso \$/ rmany1 (try (spacePreferred */ betweenParens (caseBranch expr)))
                                    \*/ alternatives [ try (justIso \$/ (spacePreferred */ expr))
                                                     , rpure Nothing
                                                     ]

    caseAnalysisIso :: Iso (CommentedExpr,CaseBranches CommentedExpr CommentedMatchArg) (Case CommentedExpr CommentedMatchArg)
    caseAnalysisIso = Iso
      { _forwards  = \(scrutinee, branches) -> Just (Case scrutinee branches)
      , _backwards = \(Case scrutinee branches) -> Just (scrutinee, branches)
      }

    defaultOnlyIso :: Iso CommentedExpr (CaseBranches CommentedExpr CommentedMatchArg)
    defaultOnlyIso = Iso
      { _forwards  = Just . DefaultOnly
      , _backwards = \c -> case c of
          DefaultOnly e
            -> Just e
          _ -> Nothing
      }

    branchesAndOptionalDefaultIso :: Iso ([CaseBranch CommentedExpr CommentedMatchArg], Maybe CommentedExpr) (CaseBranches CommentedExpr CommentedMatchArg)
    branchesAndOptionalDefaultIso = Iso
      { _forwards  = \(neBranches,mDefault) -> Just $ CaseBranches (NE.fromList neBranches) mDefault
      , _backwards = \c -> case c of
          CaseBranches neBranches mDefault
            -> Just (NE.toList neBranches,mDefault)
          _ -> Nothing
      }

    justIso :: Iso a (Maybe a)
    justIso = Iso
      {_forwards  = Just . Just
      ,_backwards = id
      }

-- A single case branch is a matchArg pattern, then a result expression
-- E.G.
-- | (?) (0)
caseBranch
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedExpr
  -> Grammar (CaseBranch CommentedExpr CommentedMatchArg)
caseBranch exprI =
  (textIs "|") */                                        -- A token bar character followed by
  (caseBranchIso \$/ (spaceAllowed   */ (sub matchArgI)) -- a matchArg to match the expression
                 \*/ (spacePreferred */ exprI))          -- and the resulting expression if the match succeeds.


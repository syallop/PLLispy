{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , FlexibleContexts
  #-}
{-|
Module      : PLLispy.Case
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr.Case with a lisp-like syntax.
-}
module PLLispy.Case
  ( caseBody
  )
  where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.List.NonEmpty

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.Pattern
import PLLispy.Case.Iso
import PLLispy.Expr.Dep
import PLLispy.Pattern.Dep
import PLLispy.Kind
import PLLispy.Type
import PLLispy.Level

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Pattern
import PL.Type
import PL.Var
import PL.TyVar

-- A Case body is made up of:
-- - A Scrutinee expression
-- - Either:
--   - A default expression
--   - One or many branches and an optional default expression
caseBody
  :: forall phase
   . ( Implicits phase
     , PatternImplicits phase
     , Constraints phase
     , PatternConstraints phase
     )
  => Grammar (ExprFor phase)
  -> Grammar (Case (ExprFor phase) (PatternFor phase))
caseBody expr =
  caseAnalysisIso \$/ expr
                  \*/ alternatives [ try defaultOnly
                                   , branchesAndOptionalDefault
                                   ]
  where
    defaultOnly :: Grammar (CaseBranches (ExprFor phase) (PatternFor phase))
    defaultOnly = defaultOnlyIso \$/ (spacePreferred */ expr)

    branchesAndOptionalDefault :: Grammar (CaseBranches (ExprFor phase) (PatternFor phase))
    branchesAndOptionalDefault =
      branchesAndOptionalDefaultIso \$/ rmany1 (try (spacePreferred */ betweenParens (caseBranch expr)))
                                    \*/ alternatives [ try (justIso \$/ (spacePreferred */ expr))
                                                     , rpure Nothing
                                                     ]

    caseAnalysisIso :: Iso (ExprFor phase, CaseBranches (ExprFor phase) (PatternFor phase)) (Case (ExprFor phase) (PatternFor phase))
    caseAnalysisIso = Iso
      { _forwards  = \(scrutinee, branches) -> Just (Case scrutinee branches)
      , _backwards = \(Case scrutinee branches) -> Just (scrutinee, branches)
      }

    defaultOnlyIso :: Iso (ExprFor phase) (CaseBranches (ExprFor phase) (PatternFor phase))
    defaultOnlyIso = Iso
      { _forwards  = Just . DefaultOnly
      , _backwards = \c -> case c of
          DefaultOnly e
            -> Just e
          _ -> Nothing
      }

    branchesAndOptionalDefaultIso :: Iso ([CaseBranch (ExprFor phase) (PatternFor phase)], Maybe (ExprFor phase)) (CaseBranches (ExprFor phase) (PatternFor phase))
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

    -- A single case branch is a pattern pattern, then a result expression
    -- E.G.
    -- | (?) (0)
    caseBranch
      :: Grammar (ExprFor phase)
      -> Grammar (CaseBranch (ExprFor phase) (PatternFor phase))
    caseBranch exprI =
      (textIs "|") */
      (caseBranchIso \$/ (spaceAllowed   */ (sub patternI))
                     \*/ (spacePreferred */ exprI))


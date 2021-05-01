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

import qualified Data.List.NonEmpty as NE

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.Pattern
import PLLispy.Case.Iso
import PLLispy.Expr.Dep
import PLLispy.Pattern.Dep
import PLLispy.Level
import PLLabel

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Pattern

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
caseBody expr = label (enhancingLabel "Case Body") $
  caseAnalysisIso \$/ expr
                  \*/ alternatives [ try defaultOnly
                                   , branchesAndOptionalDefault
                                   ]
  where
    defaultOnly :: Grammar (CaseBranches (ExprFor phase) (PatternFor phase))
    defaultOnly = label (enhancingLabel "Default Only") $ defaultOnlyIso \$/ (spacePreferred */ expr)

    branchesAndOptionalDefault :: Grammar (CaseBranches (ExprFor phase) (PatternFor phase))
    branchesAndOptionalDefault = label (enhancingLabel "Branches with optional Default") $
      branchesAndOptionalDefaultIso \$/ rmany1 (try (spacePreferred */ betweenParens (caseBranch expr)))
                                    \*/ alternatives [ try (justIso \$/ (spacePreferred */ expr))
                                                     , rpure Nothing
                                                     ]

    caseAnalysisIso :: Iso (ExprFor phase, CaseBranches (ExprFor phase) (PatternFor phase)) (Case (ExprFor phase) (PatternFor phase))
    caseAnalysisIso = Iso
      { _forwards  = \(scrutinee, branches) -> Just (Case scrutinee branches)
      , _backwards = \(Case scrutinee branches) -> Just (scrutinee, branches)
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
    caseBranch exprI = label (enhancingLabel "Case Branch") $
      (textIs "|") */
      (caseBranchIso \$/ (spaceAllowed   */ (sub patternI))
                     \*/ (spacePreferred */ exprI))


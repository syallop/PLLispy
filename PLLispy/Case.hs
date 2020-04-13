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

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var

-- A Case body is made up of:
-- - A Scrutinee expression
-- - Either:
--   - One or many branches and an optional default expression
--   - A default expression
caseBody
  :: ( Show b
     , Show abs
     , Show tb
     , Ord tb
     , Eq b
     , Eq abs
     , ?eb :: Grammar b
     , ?tb :: Grammar tb
     )
  => Grammar (Expr b abs tb)
  -> Grammar (Case (Expr b abs tb) (MatchArg b tb))
caseBody expr =
  caseAnalysisIso \$/ (parensPreferred expr)                                                      -- The scrutinee expression
                   \*/ (rmany (try (spaceRequired */ (parensPreferred $ caseBranch expr))))       -- Zero or many case branches. Try allows us to have 0 matches and unconsume spaces and parens that might be part of the default branch.
                   \*/ (alternatives [ try $ justIso \$/ (spaceRequired */ parensPreferred expr)  -- An optional default branch. The entire expression is invalid if no branches or a default are supplied.
                                     , rpure Nothing
                                     ])
  where
    justIso :: Iso a (Maybe a)
    justIso = Iso
      {_forwards  = Just . Just
      ,_backwards = id
      }

    caseAnalysisIso :: Iso (Expr b abs tb
                           ,([CaseBranch (Expr b abs tb) (MatchArg b tb)]
                            ,Maybe (Expr b abs tb)
                            )
                           )
                           (Case (Expr b abs tb) (MatchArg b tb))
    caseAnalysisIso = Iso
      {_forwards = \(scrutinee,(branches,mDefault)) -> case (branches,mDefault) of
         ([], Just d)
           -> Just $ Case scrutinee $ DefaultOnly d

         -- No branches or default provided
         ([],Nothing)
           -> Nothing

         (bs,_)
           -> Just $ Case scrutinee $ CaseBranches (NE.fromList bs) mDefault

      ,_backwards = \expr -> case expr of
          (Case scrutinee someBranches)
            -> case someBranches of
                 DefaultOnly def
                   -> Just $ (scrutinee,([],Just def))
                 CaseBranches bs mDef
                   -> Just $ (scrutinee,(NE.toList bs,mDef))
          _
            -> Nothing
      }


-- A single case branch is a matchArg pattern, then a result expression
-- E.G.
-- | (?) (0)
caseBranch
  :: ( Show b
     , Show abs
     , Show tb
     , Ord tb
     , Eq b
     , Eq abs
     , ?eb :: Grammar b
     , ?tb :: Grammar tb
     )
  => Grammar (Expr b abs tb)
  -> Grammar (CaseBranch (Expr b abs tb) (MatchArg b tb))
caseBranch exprI =
  (textIs "|") */                                                 -- A token bar character followed by
  (caseBranchIso \$/ (spacePreferred */ parensPreferred matchArg) -- a matchArg to match the expression
                 \*/ (spaceRequired */ parensPreferred exprI))    -- and the resulting expression if the match succeeds.


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

-- An entire case statement starts with "CASE" followed by a scrutinee then the
-- case branches. This grammar is everything but the "CASE" token.
-- (\Scrut 0)
-- ((|? (\Matchedfoo 0))
-- (|? (\Matchedbar 0)))
--     (\Default 0)
-- or
-- (\Scrut 0)
--   (((|? (\Matchedfoo 0))
--     (|? (\Matchedbar 0)))
--         (\Default 0))
caseStatement
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
caseStatement exprGr
  = caseIso \$/ exprGr           -- The scrutinee expression is given first followed by
            \*/ caseBody' exprGr -- a space then the case body.
  where
    caseBody' exprGr
      = token $ alternatives
          [ caseBody exprGr
          , betweenParens $ caseBody' exprGr
          ]

-- Either someCaseBranches or
-- ((|? (\Far 0))
--  (|? (\Bar 0)))
--      (\Baz 0)
-- or
-- \Foo 0
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
  -> Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBody scrutineeGr =
  token $ alternatives
    [ caseBranches scrutineeGr         -- case branches with an optional default
    , defaultOnly scrutineeGr              -- or just a default
    , betweenParens $ caseBody scrutineeGr
    ]

-- TODO: Ditch some of the tries below this point/ rework cases. Perhaps delete
-- the default branch and use a match of (|? ...)

-- One or many casebranch then a possible default expr
-- ((|? (\Far 0))
--  (|? (\Bar 0)))
--      (\Baz 0)
caseBranches
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
  -> Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
caseBranches scrutineeGr =
  caseBranchesIso
    \$/ someCaseBranches' scrutineeGr -- some case branches
    \*/ optionalDefaultBranch         -- and an optional default
  where
    someCaseBranches' scrutineeGr =
      token $ alternatives
        [ try $ someCaseBranches scrutineeGr
        , try $ betweenParens $ someCaseBranches' scrutineeGr
        ]

    optionalDefaultBranch =
      try (justI \$/ scrutineeGr) \|/ rpure Nothing

    justI :: Iso a (Maybe a)
    justI = Iso
      (\a -> Just $ Just a)
      id

-- A non-empty list of caseBranch
-- (|? (\Foo 0)) (|? (\Bar 0))
someCaseBranches
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
  -> Grammar (NonEmpty (CaseBranch (Expr b abs tb) (MatchArg b tb)))
someCaseBranches exprI =
  nonEmptyI
    \$/ (try $ caseBranch exprI)         -- One required case branch
    \*/ (try $ rmany $ try $ caseBranch exprI) -- 0-many more case branches
  where
    nonEmptyI :: Iso (a,[a]) (NonEmpty a)
    nonEmptyI = Iso
      (\(a,as) -> Just $ a :| as)
      (\ne -> let (a,mNE) = NE.uncons ne
                 in Just (a,maybe [] NE.toList mNE)
      )

-- A single case branch is a matchArg pattern, then a result expression
-- E.G.: |? (\Foo 0)
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
  token $ alternatives
    [ try $ caseBranch'               -- A single case branch
    , try $ betweenParens caseBranch'
    ]
  where
    caseBranch' =
      charIs '|' */                -- A token bar character followed by
      (caseBranchIso \$/ matchArg  -- a matchArg to match the expression
                     \*/ exprI)    -- and the resulting expression if the match succeeds.

-- A default case branch only
defaultOnly
  :: ( Show b
     , Show abs
     , Show tb
     , Ord tb
     , Eq b
     , Eq abs
     )
  => Grammar (Expr b abs tb)
  -> Grammar (CaseBranches (Expr b abs tb) (MatchArg b tb))
defaultOnly exprI =
  defaultOnlyIso \$/ exprI -- The default branch is just an expression.


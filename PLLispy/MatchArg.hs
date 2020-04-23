{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PLLispy.MatchArg
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr.Expr with a lisp-like syntax.
-}
module PLLispy.MatchArg where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.List.NonEmpty

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.MatchArgIso
import PLLispy.Kind
import PLLispy.Type
import PLLispy.Level

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var
import PL.TyVar

matchArgI
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Level
  -> Grammar MatchArg
matchArgI = level unambiguous ambiguous
  where
    unambiguous :: [Grammar MatchArg]
    unambiguous =
      [ bind
      , matchBinding
      ]

    ambiguous :: [Grammar MatchArg]
    ambiguous =
      [ matchSum
      , matchProduct
      , matchUnion
      ]

matchArg
  :: Grammar Var
  -> Grammar TyVar
  -> Level
  -> Grammar MatchArg
matchArg eb tb n =
  let ?eb = eb
      ?tb = tb
   in matchArgI n

-- A plus followed by an index and a matchArg
-- E.G.: +0 ?
matchSum
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar MatchArg
matchSum =
  plus */                                           -- A token plus character followed by
  (matchSumIso \$/ (spaceAllowed */ natural)        -- the index into the sum type
               \*/ spacePreferred */ sub matchArgI) -- then the match for that type.

-- A star followed by zero or more matchArgs
matchProduct
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar MatchArg
matchProduct =
  star */                                                                      -- A token star character followed by
  (matchProductIso \$/ (spaceAllowed */ sepBy spacePreferred (sub matchArgI))) -- a match for each component of the product

-- A union followed by a type index and a matchArg
matchUnion
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar MatchArg
matchUnion =
  union */                                              -- A union character followed by
  (matchUnionIso \$/ (spaceAllowed   */ sub typI)       -- the type index into a union type
                 \*/ (spacePreferred */ sub matchArgI)) -- then the match for that type.

-- A var
matchBinding
  :: ( ?eb :: Grammar Var
     )
  => Grammar MatchArg
matchBinding =
  matchBindingIso \$/ ?eb -- Match the var binding by the provided grammar.

-- A '?'
bind :: Grammar MatchArg
bind =
  question */                 -- A question character indicates an expression is to be bound.
  (matchBindIso \$/ rpure ())


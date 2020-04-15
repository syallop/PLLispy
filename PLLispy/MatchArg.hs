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

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var

matchArg
  :: ( Show b
     , Show tb
     , Ord tb
     , Eq b
     , ?eb :: Grammar b
     , ?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchArg
  = token $ alternatives
    [ bind
    , matchBinding ?eb
    , matchSum
    , matchProduct
    , matchUnion
    , betweenParens matchArg
    ]

parensMatchArg
  :: ( Show b
     , Show tb
     , Ord tb
     , Eq b
     , ?eb :: Grammar b
     , ?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
parensMatchArg = alternatives
  [ bind
  , matchBinding ?eb
  , parensPreferred matchArg
  ]

-- A plus followed by an index and a matchArg
-- E.G.: +0 ?
matchSum
  :: ( Show b
     , Show tb
     , Ord tb
     , Eq b
     , ?eb :: Grammar b
     , ?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchSum =
  plus */                                      -- A token plus character followed by
  (matchSumIso \$/ (spaceAllowed */ natural)   -- the index into the sum type
               \*/ spacePreferred */ parensMatchArg) -- then the match for that type.

-- A star followed by zero or more matchArgs
matchProduct
  :: ( Show b
     , Show tb
     , Ord tb
     , Eq b
     , ?eb :: Grammar b
     , ?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchProduct =
  star */                                                                     -- A token star character followed by
  (matchProductIso \$/ (spaceAllowed */ sepBy spacePreferred parensMatchArg)) -- a match for each component of the product

-- A union followed by a type index and a matchArg
matchUnion
  :: ( Show b
     , Show tb
     , Ord tb
     , Eq b
     , ?eb :: Grammar b
     , ?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchUnion =
  union */                                               -- A union character followed by
  (matchUnionIso \$/ (spaceAllowed   */ parensTyp ?tb)   -- the type index into a union type
                 \*/ (spacePreferred */ parensMatchArg)) -- then the match for that type.

-- A var
matchBinding
  :: ( Show b
     , Show tb
     )
  => Grammar b
  -> Grammar (MatchArg b tb)
matchBinding eb =
  matchBindingIso \$/ eb -- Match the var binding by the provided grammar.

-- A '?'
bind
  :: ( Show b
     , Show tb
     , Eq b
     , Eq tb
     )
  => Grammar (MatchArg b tb)
bind =
  question */                 -- A question character indicates an expression is to be bound.
  (matchBindIso \$/ rpure ())


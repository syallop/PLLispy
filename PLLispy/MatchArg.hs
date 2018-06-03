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
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Eq b
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchArg
  = bind
 \|/ (matchBinding ?eb)
 \|/ matchSum
 \|/ matchProduct
 \|/ matchUnion
 \|/ (try $ betweenParens matchArg)

-- A plus followed by an index and a matchArg
-- E.G.: +0 ?
matchSum
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Eq b
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchSum = plus */ (matchSumIso \$/ natural \*/ (spaceRequired */ matchArg))

-- A star followed by zero or more matchArgs
matchProduct
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Eq b
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchProduct = star */ (matchProductIso \$/ rmany (spaceRequired */ matchArg))

-- A union followed by a type index and a matchArg
matchUnion
  :: (Show b
     ,Show tb
     ,Ord tb
     ,Eq b
     ,?eb :: Grammar b
     ,?tb :: Grammar tb
     )
  => Grammar (MatchArg b tb)
matchUnion = union */ (matchUnionIso \$/ typ ?tb \*/ (spaceRequired */ matchArg))

-- A var
matchBinding
  :: (Show b, Show tb)
  => Grammar b
  -> Grammar (MatchArg b tb)
matchBinding eb = matchBindingIso \$/ eb

-- A '?'
bind
  :: (Show b
     ,Show tb
     ,Eq b
     ,Eq tb
     )
  => Grammar (MatchArg b tb)
bind = question */ (matchBindIso \$/ rpure ())


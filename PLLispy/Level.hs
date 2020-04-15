{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PLLispy.Expr
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Exports combinators for working with Grammars that are interpreted differently
depending on whether they are at a top or sub level.

-}
module PLLispy.Level where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text

import PLGrammar
import Reversible
import Reversible.Iso

import PLLabel

-- A Grammar is either at the Top level or some unknown Sub level.
data Level
  = Top
  | Sub
  deriving Show

-- Top Level Grammars are:
-- - All Grammars (ambiguous and unambiguous).
-- - Allow surrounding parenthesis (but do not prefer them)
--
-- Sub Level Grammars are:
-- - All unambiguous Grammars _without_ parenthesis
-- - Or a Top Level grammar with parenthesis preferred.
level
  :: Show a
  => [Grammar a]
  -> [Grammar a]
  -> (Level -> Grammar a)
level unambiguous ambiguous = \l -> case l of
  Sub
    -> subLevel
  Top
    -> topLevel
  where
    topLevel = alternatives $ ambiguous <> unambiguous <> [betweenParens topLevel]
    subLevel = alternatives $ unambiguous <> [parensPreferred topLevel]

top :: (Level -> Grammar a) -> Grammar a
top f = f Top

sub :: (Level -> Grammar a) -> Grammar a
sub f = f Sub

-- Forwards: Parenthesis are allowed but not required
-- Backwards: Parenthesis are used
parensPreferred
  :: (Show a)
  => Grammar a
  -> Grammar a
parensPreferred g = alternatives
  [ try $ textIs "(" */ g \* textIs ")"
  , g
  ]


{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLLispy.Kind
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Kind with a lisp-like syntax.
-}
module PLLispy.Kind where

import Control.Applicative

import PL.Kind

import PLLispy.Kind.Iso
import PLLispy.Level

import PLGrammar
import Reversible
import Reversible.Iso

kindAbs :: Grammar Kind
kindAbs = kind

kind :: Grammar Kind
kind =  token $ alternatives $
  [ simpleKind
  , arrowKind
  , betweenParens kind
  ]

parensKind :: Grammar Kind
parensKind = alternatives
  [ simpleKind
  , parensPreferred kind
  ]

simpleKind :: Grammar Kind
simpleKind = textIs "KIND" */ rpure Kind

arrowKind :: Grammar Kind
arrowKind = arrow */ spaceAllowed */ (kindArrowIso \$/ parensKind \*/ (spacePreferred */ parensKind))


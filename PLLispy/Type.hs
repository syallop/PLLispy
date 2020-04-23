{-# LANGUAGE ScopedTypeVariables, ImplicitParams, OverloadedStrings #-}
{-|
Module      : PLLispy.Type
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Type with a lisp-like syntax.
-}
module PLLispy.Type where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import PLGrammar
import Reversible.Iso

import PLLispy.TypeIso
import PLLispy.Kind
import PLLispy.Level

import PL.Kind
import PL.Name
import PL.TyVar
import PL.Type hiding (arrowise)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

nonEmptyIso :: Iso [a] (NonEmpty a)
nonEmptyIso = Iso
  {_forwards = \xs -> case xs of
                 []
                   -> Nothing

                 (head:tail)
                   -> Just $ head NE.:| tail
  ,_backwards = Just . NE.toList
  }

tyVar
  :: Grammar TyVar
tyVar =
  charIs '?' */
  (tyVarIso \$/ natural)

-- A name is an uppercase followed by zero or more lower case characters
name :: Grammar Text.Text
name = nameIso \$/ upper \*/ longestMatching isLower

typeName :: Grammar TypeName
typeName = typeNameIso \$/ name

-- A named type is just a name which appears in the type position
namedTyp :: Grammar Type
namedTyp = namedIso \$/ typeName

-- A plus followed by zero or more types
sumTyp :: (?tb :: Grammar TyVar) => Grammar Type
sumTyp =
  plus */
  (sumTIso \$/ (spaceAllowed */ (sepBy1 spacePreferred $ sub typI)))

-- A star followed by zero or more types
productTyp :: (?tb :: Grammar TyVar) => Grammar Type
productTyp =
  star */
  (productTIso \$/ (spaceAllowed */ sepBy spacePreferred (sub typI)))

-- A union followed by zero or more types
unionTyp :: (?tb :: Grammar TyVar) => Grammar Type
unionTyp =
  union */
  (unionTIso \$/ (setIso \$/ (spaceAllowed */ sepBy spacePreferred (sub typI))))

-- An arrow followed by two types
arrowTyp :: (?tb :: Grammar TyVar) => Grammar Type
arrowTyp =
  arrow */ (arrowIso \$/ (spaceAllowed */ sub typI)
                     \*/ (spacePreferred */ sub typI))

-- A big arrow followed by a Kind and a Type
bigArrowTyp :: (?tb :: Grammar TyVar) => Grammar Type
bigArrowTyp =
  bigArrow */ (bigArrowIso \$/ (spaceAllowed */ parensKind)
                           \*/ (spacePreferred */ sub typI))
  where
    -- TODO:
    -- - Better ascii symbol
    -- - Unicode symbol
    -- If Lambda       '\'  has type '->'
    -- Then BigLambda  '/\' has type '/->'
    bigArrow = textIs "/->"

-- A type-lambda followed by an abstracted kind, then a type
typeLamTyp :: (?tb :: Grammar TyVar) => Grammar Type
typeLamTyp =
  bigLambda */ (typeLamIso \$/ (spaceAllowed */ (parensPreferred kindAbs))
                           \*/ (spacePreferred */ sub typI))

-- An type-app followed by two types
typeAppTyp :: (?tb :: Grammar TyVar) => Grammar Type
typeAppTyp =
  bigApp */ (typeAppIso \$/ (spaceAllowed */ sub typI)
                        \*/ (spacePreferred */ sub typI))
  where
    bigApp = textIs "/@"

-- Given a parser for the type of binding used in types, parse a type binding
typeBindingTyp :: Grammar TyVar -> Grammar Type
typeBindingTyp gtb = typeBindingIso \$/ gtb

typI
  :: (?tb :: Grammar TyVar)
  => Level
  -> Grammar Type
typI = level unambiguousTypI ambiguousTypI
  where
    unambiguousTypI :: [Grammar Type]
    unambiguousTypI =
      [ namedTyp
      , typeBindingTyp ?tb
      ]

    ambiguousTypI :: [Grammar Type]
    ambiguousTypI =
      [ typeLamTyp
      , typeAppTyp
      , arrowTyp
      , sumTyp
      , productTyp
      , unionTyp
      , bigArrowTyp
      ]

typ :: Grammar TyVar
    -> Level
    -> Grammar Type
typ tb = let ?tb = tb in typI

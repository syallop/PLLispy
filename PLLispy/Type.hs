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
namedTyp :: (Ord tb,Show tb) => Grammar (Type tb)
namedTyp = namedIso \$/ typeName

-- A plus followed by zero or more types
sumTyp :: (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
sumTyp =
  plus */
  (sumTIso \$/ (spaceAllowed */ (sepBy1 spacePreferred $ sub typI)))

-- A star followed by zero or more types
productTyp :: (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
productTyp =
  star */
  (productTIso \$/ (spaceAllowed */ sepBy spacePreferred (sub typI)))

-- A union followed by zero or more types
unionTyp :: forall tb. (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
unionTyp =
  union */
  (unionTIso \$/ (setIso \$/ (spaceAllowed */ sepBy spacePreferred (sub typI))))

-- An arrow followed by two types
arrowTyp :: (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
arrowTyp =
  arrow */ (arrowIso \$/ (spaceAllowed */ sub typI)
                     \*/ (spacePreferred */ sub typI))

-- A big arrow followed by a Kind and a Type
bigArrowTyp :: (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
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
typeLamTyp :: forall tb. (Ord tb,Show tb,?tb :: Grammar tb) => Grammar (Type tb)
typeLamTyp =
  bigLambda */ (typeLamIso \$/ (spaceAllowed */ (parensPreferred kindAbs))
                           \*/ (spacePreferred */ sub typI))

-- An type-app followed by two types
typeAppTyp :: (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
typeAppTyp =
  bigApp */ (typeAppIso \$/ (spaceAllowed */ sub typI)
                        \*/ (spacePreferred */ sub typI))
  where
    bigApp = textIs "/@"

-- Given a parser for the type of binding used in types, parse a type binding
typeBindingTyp :: Show tb => Grammar tb -> Grammar (Type tb)
typeBindingTyp gtb = typeBindingIso \$/ gtb

typI
  :: forall tb. (Show tb,Ord tb,?tb :: Grammar tb)
  => Level
  -> Grammar (Type tb)
typI = level unambiguousTypI ambiguousTypI
  where
    unambiguousTypI :: [Grammar (Type tb)]
    unambiguousTypI =
      [ namedTyp
      , typeBindingTyp ?tb
      ]

    ambiguousTypI :: [Grammar (Type tb)]
    ambiguousTypI =
      [ typeLamTyp
      , typeAppTyp
      , arrowTyp
      , sumTyp
      , productTyp
      , unionTyp
      , bigArrowTyp
      ]

typ :: (Show tb, Ord tb)
    => Grammar tb
    -> Level
    -> Grammar (Type tb)
typ tb = let ?tb = tb in typI

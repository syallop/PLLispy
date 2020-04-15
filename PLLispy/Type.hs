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
sumTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
sumTyp tb =
  plus */
  (sumTIso \$/ (spaceRequired */ (sepBy1 spacePreferred $ parensTyp tb)))

-- A star followed by zero or more types
productTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
productTyp tb =
  star */
  (productTIso \$/ rmany (spaceRequired */ parensTyp tb))

-- A union followed by zero or more types
unionTyp :: forall tb. (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
unionTyp tb =
  union */
  (unionTIso \$/ (setIso \$/ rmany (spaceRequired */ parensTyp tb)))

-- An arrow followed by two types
arrowTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
arrowTyp tb =
  arrow */ (arrowIso \$/ (spacePreferred */ parensTyp tb)
                     \*/ (spaceRequired */ parensTyp tb))

-- A big arrow followed by a Kind and a Type
bigArrowTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
bigArrowTyp tb =
  bigArrow */ (bigArrowIso \$/ (spacePreferred */ (parensPreferred kind))
                           \*/ (spaceRequired */ parensTyp tb))
  where
    -- TODO:
    -- - Better ascii symbol
    -- - Unicode symbol
    -- If Lambda       '\'  has type '->'
    -- Then BigLambda  '/\' has type '/->'
    bigArrow = textIs "/->"

-- A type-lambda followed by an abstracted kind, then a type
typeLamTyp :: forall tb. (Ord tb,Show tb) => Grammar tb -> Grammar (Type tb)
typeLamTyp tb =
  bigLambda */ (typeLamIso \$/ (spacePreferred */ (parensPreferred kindAbs))
                           \*/ (spaceRequired */ parensTyp tb))

-- An type-app followed by two types
typeAppTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
typeAppTyp tb =
  bigApp */ (typeAppIso \$/ (spacePreferred */ parensTyp tb)
                        \*/ (spaceRequired */ parensTyp tb))
  where
    bigApp = textIs "/@"

-- Given a parser for the type of binding used in types, parse a type binding
typeBindingTyp :: Show tb => Grammar tb -> Grammar (Type tb)
typeBindingTyp gtb = typeBindingIso \$/ gtb

typI :: (Show tb,Ord tb,?tb :: Grammar tb) => Grammar (Type tb)
typI = let tb = ?tb in typ tb

-- A type is one of several variants, and may be nested in parenthesis
typ :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
typ tb = token $ alternatives
  [ typeLamTyp tb
  , typeAppTyp tb
  , arrowTyp tb
  , sumTyp tb
  , productTyp tb
  , unionTyp tb
  , namedTyp
  , typeBindingTyp tb
  , bigArrowTyp tb
  , betweenParens $ typ tb
  ]

parensTyp :: (Show tb,Ord tb) => Grammar tb -> Grammar (Type tb)
parensTyp tb = alternatives
  [ namedTyp
  , typeBindingTyp tb
  , parensPreferred (typ tb)
  ]

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


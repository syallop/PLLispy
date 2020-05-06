{-# LANGUAGE
    ScopedTypeVariables
  , ImplicitParams
  , OverloadedStrings
  , FlexibleContexts
  , MultiWayIf
  #-}
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
import PL.Commented
import PL.TyVar
import PL.FixPhase
import PL.Expr
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

-- | A name is an uppercase followed by zero or more lower case characters.
--
-- - "U" is not a name as we use it to denote unions.
name :: Grammar Text.Text
name = try $ nameIso \$/ charWhen upperAlpha \*/ longestMatching lowerAlpha
  where
    reservedWords :: [Text.Text]
    reservedWords = ["U"]

    isntReserved :: Text.Text -> Bool
    isntReserved = not . (`elem` reservedWords)

    upperAlpha = (`elem` ['A'..'Z'])
    lowerAlpha = (`elem` ['a'..'z'])

    valid :: Char -> Text.Text -> Bool
    valid c cs = and [isUpper c, Text.all isLower cs, isntReserved (Text.cons c cs)]

    nameIso :: Iso (Char, Text.Text) Text.Text
    nameIso = Iso
      {_forwards = \(c,cs) -> if
                   | valid c cs
                    -> Just $ Text.cons c cs

                   | otherwise
                     -> Nothing

      ,_backwards = \txt -> case Text.uncons txt of
                              Just (c,cs)
                                | valid c cs
                                 -> Just (c,cs)

                              otherwise
                                -> Nothing
      }


typeName :: Grammar TypeName
typeName = typeNameIso \$/ name

-- A named type is just a name which appears in the type position
namedTyp :: Grammar CommentedType
namedTyp = namedIso \$/ typeName

-- A plus followed by zero or more types
sumTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
sumTyp =
  plus */
  (sumTIso \$/ (spaceAllowed */ (sepBy1 spacePreferred $ sub typI)))

-- A star followed by zero or more types
productTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
productTyp =
  star */
  (productTIso \$/ (spaceAllowed */ sepBy spacePreferred (sub typI)))

-- A union followed by zero or more types
unionTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
unionTyp =
  union */
  (unionTIso \$/ (spaceAllowed */ (setIso \$/ sepBy spacePreferred (sub typI))))

-- An arrow followed by two types
arrowTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
arrowTyp =
  arrow */ (arrowIso \$/ (spaceAllowed */ sub typI)
                     \*/ (spacePreferred */ sub typI))

-- A big arrow followed by a Kind and a Type
bigArrowTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
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
typeLamTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
typeLamTyp =
  bigLambda */ (typeLamIso \$/ (spaceAllowed */ (parensPreferred kindAbs))
                           \*/ (spacePreferred */ sub typI))

-- An type-app followed by two types
typeAppTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
typeAppTyp =
  bigApp */ (typeAppIso \$/ (spaceAllowed */ sub typI)
                        \*/ (spacePreferred */ sub typI))
  where
    bigApp = textIs "/@"

-- Given a parser for the type of binding used in types, parse a type binding
typeBindingTyp :: Grammar TyVar -> Grammar CommentedType
typeBindingTyp gtb = typeBindingIso \$/ gtb

-- A Commented type
commentedTyp :: (?tb :: Grammar TyVar) => Grammar CommentedType
commentedTyp =
  charIs '"' */
  (commentedIso \$/ (commentText \* charIs '"')
                \*/ (spaceAllowed */ sub typI)
  )
  where
    commentText :: Grammar Comment
    commentText = commentTextIso \$/ longestMatching isCommentChar

    -- TODO: Use a better character class here.
    isCommentChar :: Char -> Bool
    isCommentChar c = c /= '\"'

    commentTextIso :: Iso Text.Text Comment
    commentTextIso = Iso
      {_forwards  = Just . Comment
      ,_backwards = \(Comment t) -> Just t
      }

typI
  :: (?tb :: Grammar TyVar)
  => Level
  -> Grammar CommentedType
typI = level unambiguousTypI ambiguousTypI
  where
    unambiguousTypI :: [Grammar CommentedType]
    unambiguousTypI =
      [ namedTyp
      , typeBindingTyp ?tb
      , commentedTyp
      ]

    ambiguousTypI :: [Grammar CommentedType]
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
    -> Grammar CommentedType
typ tb = let ?tb = tb in typI

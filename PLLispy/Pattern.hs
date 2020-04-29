{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PLLispy.Pattern
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr.Expr with a lisp-like syntax.
-}
module PLLispy.Pattern where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.List.NonEmpty
import Data.Text (Text)

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.PatternIso
import PLLispy.Kind
import PLLispy.Type
import PLLispy.Level

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Type
import PL.Var
import PL.TyVar

patternI
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Level
  -> Grammar CommentedPattern
patternI = level unambiguous ambiguous
  where
    unambiguous :: [Grammar CommentedPattern]
    unambiguous =
      [ bind
      , bindingPattern
      , commentedPattern
      ]

    ambiguous :: [Grammar CommentedPattern]
    ambiguous =
      [ sumPattern
      , productPattern
      , unionPattern
      ]

pattern
  :: Grammar Var
  -> Grammar TyVar
  -> Level
  -> Grammar CommentedPattern
pattern eb tb n =
  let ?eb = eb
      ?tb = tb
   in patternI n

-- A plus followed by an index and a pattern
-- E.G.: +0 ?
sumPattern
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedPattern
sumPattern =
  plus */                                           -- A token plus character followed by
  (sumPatternIso \$/ (spaceAllowed */ natural)        -- the index into the sum type
                 \*/ spacePreferred */ sub patternI) -- then the match for that type.

-- A star followed by zero or more patterns
productPattern
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedPattern
productPattern =
  star */                                                                      -- A token star character followed by
  (productPatternIso \$/ (spaceAllowed */ sepBy spacePreferred (sub patternI))) -- a match for each component of the product

-- A union followed by a type index and a pattern
unionPattern
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedPattern
unionPattern =
  union */                                              -- A union character followed by
  (unionPatternIso \$/ (spaceAllowed   */ sub typI)       -- the type index into a union type
                   \*/ (spacePreferred */ sub patternI)) -- then the match for that type.

-- A var
bindingPattern
  :: ( ?eb :: Grammar Var
     )
  => Grammar CommentedPattern
bindingPattern =
  bindingPatternIso \$/ ?eb -- Match the var binding by the provided grammar.

-- A '?'
bind :: Grammar CommentedPattern
bind =
  question */                 -- A question character indicates an expression is to be bound.
  (bindIso \$/ rpure ())

-- A Commented Pattern
commentedPattern
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedPattern
commentedPattern =
  charIs '"' */
  (commentedIso \$/ (commentText \* charIs '"')
                \*/ (spaceAllowed */ sub patternI)
  )
  where
    commentText :: Grammar Comment
    commentText = commentTextIso \$/ longestMatching isCommentChar

    -- TODO: Use a better character class here.
    isCommentChar :: Char -> Bool
    isCommentChar c = c /= '\"'

    commentTextIso :: Iso Text Comment
    commentTextIso = Iso
      {_forwards  = Just . Comment
      ,_backwards = \(Comment t) -> Just t
      }


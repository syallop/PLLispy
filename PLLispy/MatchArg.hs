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
import Data.Text (Text)

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.MatchArgIso
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

matchArgI
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Level
  -> Grammar CommentedMatchArg
matchArgI = level unambiguous ambiguous
  where
    unambiguous :: [Grammar CommentedMatchArg]
    unambiguous =
      [ bind
      , matchBinding
      , commentedMatchArg
      ]

    ambiguous :: [Grammar CommentedMatchArg]
    ambiguous =
      [ matchSum
      , matchProduct
      , matchUnion
      ]

matchArg
  :: Grammar Var
  -> Grammar TyVar
  -> Level
  -> Grammar CommentedMatchArg
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
  => Grammar CommentedMatchArg
matchSum =
  plus */                                           -- A token plus character followed by
  (matchSumIso \$/ (spaceAllowed */ natural)        -- the index into the sum type
               \*/ spacePreferred */ sub matchArgI) -- then the match for that type.

-- A star followed by zero or more matchArgs
matchProduct
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedMatchArg
matchProduct =
  star */                                                                      -- A token star character followed by
  (matchProductIso \$/ (spaceAllowed */ sepBy spacePreferred (sub matchArgI))) -- a match for each component of the product

-- A union followed by a type index and a matchArg
matchUnion
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedMatchArg
matchUnion =
  union */                                              -- A union character followed by
  (matchUnionIso \$/ (spaceAllowed   */ sub typI)       -- the type index into a union type
                 \*/ (spacePreferred */ sub matchArgI)) -- then the match for that type.

-- A var
matchBinding
  :: ( ?eb :: Grammar Var
     )
  => Grammar CommentedMatchArg
matchBinding =
  matchBindingIso \$/ ?eb -- Match the var binding by the provided grammar.

-- A '?'
bind :: Grammar CommentedMatchArg
bind =
  question */                 -- A question character indicates an expression is to be bound.
  (matchBindIso \$/ rpure ())

-- A Commented MatchArg
commentedMatchArg
  :: ( ?eb :: Grammar Var
     , ?tb :: Grammar TyVar
     )
  => Grammar CommentedMatchArg
commentedMatchArg =
  charIs '"' */
  (commentedIso \$/ (commentText \* charIs '"')
                \*/ (spaceAllowed */ sub matchArgI)
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


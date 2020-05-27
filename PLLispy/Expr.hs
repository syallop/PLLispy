{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-|
Module      : PLLispy.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr with a lisp-like syntax.
-}
module PLLispy.Expr where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.ExprIso
import PLLispy.Case
import PLLispy.Kind
import PLLispy.Type
import PLLispy.Name

import PLLispy.Level

import PLLabel

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Kind
import PL.Hash
import PL.Type
import PL.Name
import PL.Var
import PL.TyVar

-- Implicitly bind Grammars for expression bindings, abstractions and type bindings
-- TODO: This is probably a failed experiment.
type Implicits = (?eb :: Grammar Var,?abs :: Grammar CommentedType,?tb :: Grammar TyVar)

-- Bind the given grammars into a Grammar which takes them implicitly
using
  :: Implicits
  => Grammar Var
  -> Grammar CommentedType
  -> Grammar TyVar
  -> Grammar a
  -> Grammar a
using b abs tb a =
  let ?eb = b
      ?abs = abs
      ?tb = tb
    in a

implicitly
  :: Implicits
  => (Grammar Var -> Grammar CommentedType -> Grammar TyVar -> Grammar a)
  -> Grammar a
implicitly f = f ?eb ?abs ?tb

-- The 'Lam' lambda constructor is defined by:
lamExpr
  :: Implicits
  => Grammar CommentedExpr
lamExpr =
  lambda */                                 -- A token lambda character followed by
  (lamIso \$/ (spaceAllowed */ ?abs)        -- an abstraction
          \*/ (spaceRequired */ sub exprI)) -- then an expression preceeded by a required space.

-- The 'BigLam' big lambda constructor is defined by:
-- A big lambda followed by one or more kind abstractions then an expression.
bigLamExpr
  :: Implicits
  => Grammar CommentedExpr
bigLamExpr =
  bigLambda */                                -- A token big lambda character followed by
  (bigLamIso \$/ kind                         -- a kind
             \*/ (spaceAllowed */ sub exprI)) -- then an expression preceeded by a required space.

-- The 'App' constructor is defined by:
appExpr
  :: Implicits
  => Grammar CommentedExpr
appExpr =
  at */                                     -- A token 'at' character followed by
  (appIso \$/ (spaceAllowed  */ sub exprI)  -- an expression
          \*/ (spaceRequired */ sub exprI)) -- then another expression preceeded by a required space.

-- The 'BigApp' constructor is defined by:
bigAppExpr
  :: Implicits
  => Grammar CommentedExpr
bigAppExpr =
  bigAt */                                    -- A token 'big at' character followed by
  (bigAppIso \$/ (spaceAllowed */ sub exprI)  -- an expression
             \*/ (spaceRequired */ sub typI)) -- then a type preceeded by a required space.

-- The binding constructor is some form of reference to a value bound by a
-- lambda abstraction. It is likely to be an index or name.
bindingExpr
  :: Grammar Var
  -> Grammar CommentedExpr
bindingExpr eb = bindingIso \$/ eb

-- The content binding constructor is a name which references an expression by
-- it's content.
contentBindingExpr
  :: Grammar CommentedExpr
contentBindingExpr = contentBindingIso \$/ contentNameGrammar

-- A Commented expression
commentedExpr
  :: Implicits
  => Grammar CommentedExpr
commentedExpr =
  charIs '"' */
  (commentedIso \$/ (commentText \* charIs '"')
                \*/ (spaceAllowed */ sub exprI)
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

-- A 'Var' refers to a bound value by counting back to the lambda which
-- abstracted it. It is a natural number 0,1,2..
var
  :: Grammar Var
var =
  varIso \$/ natural -- A variable is given by a natural number.

-- The 'Sum' constructor is defined by:
sumExpr
  :: Implicits
  => Grammar CommentedExpr
sumExpr =
  plus */                                                            -- A token '+' character followed by
  (sumIso \$/ token natural                                          -- an index into overall sum type
          \*/ (spaceAllowed */ sub exprI)                            -- then the expression preceeded by a required space
          \*/ (spaceRequired */ (sepBy1 spacePreferred $ sub typI))) -- then one or many of the constituent sum types, each preceeded by a required space.

-- The 'Product' constructor is defined by:
productExpr
  :: Implicits
  => Grammar CommentedExpr
productExpr =
  star */                                                             -- A token 'star' followed by
  (productIso \$/ (spaceAllowed */ sepBy spacePreferred (sub exprI))) -- zero or many expressions, each preceeded by a required space.

-- The 'Union' constructor is defined by:
unionExpr
  :: Implicits
  => Grammar CommentedExpr
unionExpr =
  union */                                                                        -- A token 'union' followed by
  (unionIso \$/ (spaceAllowed   */ sub typI)                                      -- a type index into the overall union type
            \*/ (spacePreferred */ sub exprI)                                     -- then the expression preceeded by a required space
            \*/ (setIso \$/ (spacePreferred */ sepBy spacePreferred (sub typI)))) -- then zero or many of the constituent union types, each preceeded by a required space.

-- "CASE" signifies the start of a case statement.
--
-- It is followed by a body which contains:
-- - A Scrutinee expression
-- - Either:
--   - One or many branches and an optional default expression
--   - A default expression
caseAnalysis :: Implicits => Grammar CommentedExpr
caseAnalysis =
  textIs "CASE" */
  (caseIso \$/ (spaceRequired */ caseBody (sub exprI)))
  where
    caseIso :: Iso (Case CommentedExpr CommentedPattern) CommentedExpr
    caseIso = Iso
      {_forwards  = Just . CaseAnalysis
      ,_backwards = \e -> case e of
         CaseAnalysis c
           -> Just c
         _ -> Nothing
      }

exprI
  :: Implicits
  => Level
  -> Grammar CommentedExpr
exprI = level unambiguousExprI ambiguousExprI
  where
    unambiguousExprI :: [Grammar CommentedExpr]
    unambiguousExprI =
      [ bindingExpr ?eb
      , contentBindingExpr
      , commentedExpr
      ]

    ambiguousExprI :: [Grammar CommentedExpr]
    ambiguousExprI =
      [ lamExpr
      , bigLamExpr
      , appExpr
      , bigAppExpr
      , sumExpr
      , productExpr
      , unionExpr
      , caseAnalysis
      ]


-- Parse a top-level expression given parsers for:
-- - Expression bindings    (E.G. Var)
-- - Expression abstraction (E.G. CommentedType)
-- - Type bindings          (E.G. Var)
--
-- Unlike sub-expressions, top-level expressions do not prefer to be surrounded
-- by parenthesis
--
-- A sub-expression contained within some larger expression will prefer to be
-- surrounded by parenthesis unless the specific sub-expression is unambigous
-- (like a single integer).
expr
  :: Grammar Var
  -> Grammar CommentedType
  -> Grammar TyVar
  -> Level
  -> Grammar CommentedExpr
expr eb abs tb n
  = let ?eb  = eb
        ?abs = abs
        ?tb  = tb
       in exprI n


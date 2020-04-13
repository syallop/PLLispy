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

import PLLabel

import PL.Case
import PL.Expr hiding (appise,lamise)
import PL.FixExpr
import PL.Kind
import PL.Type
import PL.Var

-- | An 'Expr's lambda abstracts over some value which it accepts as it's argument.
-- This grammar is a simple type definition which may therefore be used in an
-- 'Expr b (Type tb) tb'.
typeAbs
  :: ( Show tb
     , Ord tb
     )
  => Grammar tb
  -> Grammar (Type tb)
typeAbs tb = typ tb

-- Implicitly bind Grammars for expression bindings, abstractions and type bindings
-- TODO: This is probably a failed experiment.
type Implicits b abs tb = (?eb :: Grammar b,?abs :: Grammar abs,?tb :: Grammar tb)

-- Bind the given grammars into a Grammar which takes them implicitly
using
  :: Grammar b
  -> Grammar abs
  -> Grammar tb
  -> (Implicits b abs tb => Grammar a)
  -> Grammar a
using b abs tb a =
  let ?eb = b
      ?abs = abs
      ?tb = tb
    in a

implicitly
  :: (Grammar b -> Grammar abs -> Grammar tb -> Grammar a)
  -> (Implicits b abs tb => Grammar a)
implicitly f = f ?eb ?abs ?tb

type Constraints b abs tb =
  ( Show b
  , Show abs
  , Show tb
  , Ord tb
  , Implicits b abs tb
  , Eq b
  , Eq abs
  )

type ExprGrammar b abs tb = Constraints b abs tb => Grammar (Expr b abs tb)

-- The 'Lam' lambda constructor is defined by:
lamExpr
  :: Constraints b abs tb
  => Grammar (Expr b abs tb)
lamExpr =
  lambda */                                             -- A token lambda character followed by
  (lamIso \$/ (spacePreferred */ parensPreferred ?abs)  -- an abstraction
          \*/ (spaceRequired */ parensPreferred exprI)) -- then an expression preceeded by a required space.

-- The 'BigLam' big lambda constructor is defined by:
-- A big lambda followed by one or more kind abstractions then an expression.
bigLamExpr
  :: Constraints b abs tb
  => Grammar (Expr b abs tb)
bigLamExpr =
  bigLambda */                             -- A token big lambda character followed by
  (bigLamIso \$/ kind                      -- a kind
             \*/ (spaceRequired */ exprI)) -- then an expression preceeded by a required space.

-- The 'App' constructor is defined by:
appExpr
  :: Constraints b abs tb
  => Grammar (Expr b abs tb)
appExpr =
  at */                                                 -- A token 'at' character followed by
  (appIso \$/ (spaceRequired */ parensPreferred exprI)  -- an expression
          \*/ (spaceRequired */ parensPreferred exprI)) -- then another expression preceeded by a required space.

-- The 'BigApp' constructor is defined by:
bigAppExpr
  :: Constraints b abs tb
  => Grammar (Expr b abs tb)
bigAppExpr =
  bigAt */                                                      -- A token 'big at' character followed by
  (bigAppIso \$/ (spaceRequired */ parensPreferred exprI)       -- an expression
             \*/ (spaceRequired */ parensPreferred (typ ?tb)))  -- then a type preceeded by a required space.

-- The binding constructor is some form of reference to a value bound by a
-- lambda abstraction. It is likely to be an index or name.
bindingExpr
  :: (Show b,Show tb,Show abs)
  => Grammar b
  -> Grammar (Expr b abs tb)
bindingExpr eb = bindingIso \$/ eb

-- A 'Var' refers to a bound value by counting back to the lambda which
-- abstracted it. It is a natural number 0,1,2..
var
  :: Grammar Var
var =
  varIso \$/ natural -- A variable is given by a natural number.

-- The 'Sum' constructor is defined by:
sumExpr
  :: Constraints b abs tb
  => Grammar (Expr b abs tb)
sumExpr =
  plus */                                                                             -- A token '+' character followed by
  (sumIso \$/ token natural                                                           -- an index into overall sum type
          \*/ (spaceRequired */ parensPreferred exprI)                                -- then the expression preceeded by a required space
          \*/ (spaceRequired */ (sepBy1 spacePreferred (parensPreferred $ typ ?tb)))) -- then one or many of the constituent sum types, each preceeded by a required space.

-- The 'Product' constructor is defined by:
productExpr
  :: Constraints b abs tb
  => Grammar (Expr b abs tb)
productExpr =
  star */                                                         -- A token 'star' followed by
  (productIso \$/ rmany (spaceRequired */ parensPreferred exprI)) -- zero or many expressions, each preceeded by a required space.

-- The 'Union' constructor is defined by:
unionExpr
  :: Constraints b abs tb
  => Grammar (Expr b abs tb)
unionExpr =
  union */                                                                       -- A token 'union' followed by
  (unionIso \$/ (spaceRequired */ parensPreferred (typ ?tb))                     -- a type index into the overall union type
            \*/ (spaceRequired */ parensPreferred exprI)                         -- then the expression preceeded by a required space
            \*/ (setIso \$/ rmany (spaceRequired */ parensPreferred (typ ?tb)))) -- then zero or many of the constituent union types, each preceeded by a required space.

-- "CASE" signifies the start of a case statement.
--
-- It is followed by a body which contains:
-- - A Scrutinee expression
-- - Either:
--   - One or many branches and an optional default expression
--   - A default expression
caseAnalysis :: (Show b,Show abs,Show tb,Ord tb,Implicits b abs tb,Eq b,Eq abs) => Grammar (Expr b abs tb)
caseAnalysis =
  textIs "CASE" */
  (caseIso \$/ (spaceRequired */ caseBody exprI))
  where
    caseIso :: Iso (Case (Expr b abs tb) (MatchArg b tb)) (Expr b abs tb)
    caseIso = Iso
      {_forwards  = Just . FixExpr . CaseAnalysis
      ,_backwards = \e -> case e of
         (FixExpr (CaseAnalysis c))
           -> Just c
         _ -> Nothing
      }

-- Parse an expression when /implicitly/ passed porsers for:
-- - ?eb  Expression bindings    (E.G. Var)
-- - ?abs Expression abstraction (E.G. Type)
-- - ?tb  Type bindings          (E.G. Var)
exprI :: Constraints b abs tb => Grammar (Expr b abs tb)
exprI = token $ alternatives
  [ lamExpr
  , bigLamExpr
  , appExpr
  , bigAppExpr
  , sumExpr
  , productExpr
  , unionExpr
  , bindingExpr ?eb
  , caseAnalysis
  , betweenParens exprI -- TODO: Drop this here?
  -- TODO:
  -- Big lambda type bindings?
  ]

-- Parse an expression given parsers for:
-- - Expression bindings    (E.G. Var)
-- - Expression abstraction (E.G. Type)
-- - Type bindings          (E.G. Var)
expr
  :: (Show b, Show abs, Show tb, Ord tb, Eq b, Eq abs)
  => Grammar b
  -> Grammar abs
  -> Grammar tb
  -> Grammar (Expr b abs tb)
expr eb abs tb
  = let ?eb  = eb
        ?abs = abs
        ?tb  = tb
       in exprI


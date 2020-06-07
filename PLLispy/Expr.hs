{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , FlexibleContexts
  , GADTs
  #-}
{-|
Module      : PLLispy.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr with a lisp-like syntax, parameterised over grammars for
dependencies such as bindings and abstractions as well as possible constructor
extensions.
-}
module PLLispy.Expr
  (
  -- * Concrete expressions
  --
  -- Grammar for a lispy expression that depends upon grammars for things such
  -- as bindings, abstractions etc.
    expr
  , GrammarDependencies (..)
  , defaultGrammarDependencies

  -- * Expr constructors
  -- Grammars for alternatives in a general 'ExprFor phase' ast that defer to implicit
  -- dependent grammars where necessary.
  , exprI
  , lamExpr
  , appExpr
  , bindingExpr
  , contentBindingExpr
  , caseAnalysisExpr
  , sumExpr
  , productExpr
  , unionExpr
  , bigLamExpr
  , bigAppExpr
  , exprExtensionExpr

  -- * Expr extension Grammars
  --
  -- Grammars for specific extensions to the Expr ast itself.
  , commentedExpr

  -- * Grammar Dependency options
  --
  -- Grammars that are likely to be used to instantiate the dependencies

  -- ** Expr bindings
  , var

  -- ** Expr abstractions
  , typ

  -- ** ContentBindings at Expr and Type level
  , shortHash
  , contentName
  )
  where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import Data.Text
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import PLLispy.Case
import PLLispy.Expr.Iso
import PLLispy.Expr.Dep
import PLLispy.Type.Dep
import PLLispy.Pattern.Dep
import PLLispy.Kind
import PLLispy.Level
import PLLispy.Name
import PLLispy.Type
import PLLispy.Pattern

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Hash
import PL.Kind
import PL.Name
import PL.TyVar
import PL.Pattern
import PL.Type
import PL.Var
import PL.HashStore
import PL.FixPhase

import PLGrammar
import PLLabel
import Reversible
import Reversible.Iso

defaultGrammarDependencies
  :: ( Constraints phase

     , Var             ~ BindingFor phase
     , ShortHash       ~ ContentBindingFor phase
     , (TypeFor phase) ~ AbstractionFor phase
     , TyVar           ~ TypeBindingFor phase
     , ShortHash       ~ TypeContentBindingFor phase

     , Void ~ LamExtension phase
     , Void ~ AppExtension phase
     , Void ~ BindingExtension phase
     , Void ~ ContentBindingExtension phase
     , Void ~ CaseAnalysisExtension phase
     , Void ~ SumExtension phase
     , Void ~ ProductExtension phase
     , Void ~ UnionExtension phase
     , Void ~ BigLamExtension phase
     , Void ~ BigAppExtension phase

     , Void ~ NamedExtension phase
     , Void ~ ArrowExtension phase
     , Void ~ SumTExtension phase
     , Void ~ ProductTExtension phase
     , Void ~ UnionTExtension phase
     , Void ~ BigArrowExtension phase
     , Void ~ TypeLamExtension phase
     , Void ~ TypeAppExtension phase
     , Void ~ TypeBindingExtension phase
     , Void ~ TypeContentBindingExtension phase

     , Void ~ SumPatternExtension phase
     , Void ~ ProductPatternExtension phase
     , Void ~ UnionPatternExtension phase
     , Void ~ BindingPatternExtension phase
     , Void ~ BindExtension phase

     , (Commented (ExprFor phase)) ~ ExprExtension phase
     , (Commented (TypeFor phase)) ~ TypeExtension phase
     , (Commented (PatternFor phase)) ~ PatternExtension phase
     )
  => GrammarDependencies phase
defaultGrammarDependencies = GrammarDependencies
  { _bindingFor                = var
  , _contentBindingFor         = shortHash
  , _abstractionFor            = sub $ typ defaultTypeGrammarDependencies
  , _exprTypeBindingFor        = tyVar
  , _exprTypeContentBindingFor = shortHash

  , _lamGrammarExtension            = voidG
  , _appGrammarExtension            = voidG
  , _bindingGrammarExtension        = voidG
  , _contentBindingGrammarExtension = voidG
  , _caseAnalysisGrammarExtension   = voidG
  , _sumGrammarExtension            = voidG
  , _productGrammarExtension        = voidG
  , _unionGrammarExtension          = voidG
  , _bigLamGrammarExtension         = voidG
  , _bigAppGrammarExtension         = voidG

  , _exprGrammarExtension = commentedExpr defaultGrammarDependencies defaultTypeGrammarDependencies defaultPatternGrammarDependencies
  }

-- | The lambda constructor takes the form:
--
-- LAMBDA ABS EXPR
lamExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
lamExpr =
  lambda */
  (lamIso \$/ lamExtension
          \*/ (spaceAllowed */ abstraction)
          \*/ (spaceRequired */ sub exprI))

-- | The Big lambda constructor takes the form:
--
-- BIGLAMBDA kind EXPR
bigLamExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
bigLamExpr =
  bigLambda */
  (bigLamIso \$/ bigLamExtension
             \*/ kind
             \*/ (spaceAllowed */ sub exprI))

-- | The function application constructor takes the form:
--
-- AT EXPR EXPR
appExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
appExpr =
  at */
  (appIso \$/ appExtension
          \*/ (spaceAllowed  */ sub exprI)
          \*/ (spaceRequired */ sub exprI))

-- | The Big function application constructor takes the form:
--
-- BIGAT EXPR TYPE
bigAppExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
bigAppExpr =
  bigAt */
  (bigAppIso \$/ bigAppExtension
             \*/ (spaceAllowed  */ sub exprI)
             \*/ (spaceRequired */ sub typI))

-- | The binding constructor is some form of reference to a value bound by a
-- lambda abstraction. It is likely to be an index of name.
--
-- It defers to the implicit binding grammar.
bindingExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
bindingExpr = bindingIso \$/ bindingExtension \*/ binding

-- | The content binding constructor is a name which references an expression by
-- it's content.
--
-- It defers to the implicit content binding grammar.
contentBindingExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
contentBindingExpr = contentBindingIso \$/ contentBindingExtension \*/ contentBinding

-- | The Sum constructor takes the form:
--
-- PLUS natural EXPR TYPE+
sumExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
sumExpr =
  plus */
  (sumIso \$/ sumExtension
          \*/ token natural
          \*/ (spaceAllowed */ sub exprI)
          \*/ (spaceRequired */ (sepBy1 spacePreferred $ sub typI)))

-- | The Product constructor takes the form:
--
-- STAR EXPR*
productExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
productExpr =
  star */
  (productIso \$/ productExtension \*/ (spaceAllowed */ sepBy spacePreferred (sub exprI)))

-- | The Union constructor takes the form:
--
-- UNION TYPE EXPR TYPE*
unionExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
unionExpr =
  union */
  (unionIso \$/ unionExtension
            \*/ (spaceAllowed   */ sub typI)
            \*/ (spacePreferred */ sub exprI)
            \*/ (setIso \$/ (spacePreferred */ sepBy spacePreferred (sub typI))))

-- | The Case constructor takes the form:
--
-- "CASE" CASEBODY
--
-- The CASEBODY contains:
-- - A Scrutinee expression
-- - Either:
--   - One or many branches and an optional default expression
--   - A default expression
caseAnalysisExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
caseAnalysisExpr =
  textIs "CASE" */
  (caseIso \$/ caseAnalysisExtension \*/ (spaceRequired */ caseBody (sub exprI)))
  where
    caseIso :: Iso (CaseAnalysisExtension phase, Case (ExprFor phase) (PatternFor phase)) (ExprFor phase)
    caseIso = Iso
      {_forwards  = \(ext,c) -> Just . CaseAnalysisExt ext $ c
      ,_backwards = \e -> case e of
         CaseAnalysisExt ext c
           -> Just (ext, c)
         _ -> Nothing
      }

-- | Defer to the implicit Grammar for expr extensions.
exprExtensionExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
exprExtensionExpr = exprExtensionIso \$/ exprExtension

-- | A Commented Expression as the expr extension constructor takes the form:
--
-- '"' COMMENT '"' EXPR
commentedExpr
  :: (Constraints phase)
  => GrammarDependencies phase
  -> TypeGrammarDependencies phase
  -> PatternGrammarDependencies phase
  -> Grammar (Commented (ExprFor phase))
commentedExpr eDep tDep pDep =
  charIs '"' */
  (commentedExprIso \$/ (commentText \* charIs '"')
                    \*/ (spaceAllowed */ sub (expr eDep tDep pDep))
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

-- | Parse an expression given grammar dependencies for things such as:
-- - Expression bindings    (E.G. var)
-- - Expression abstraction (E.G. typ)
-- - Type bindings          (E.G. tyVar)
--
-- and a level signifier.
--
-- Top-level expressions prefer not to be surrounded by parenthesis.
--
-- Nested sub-expressions will prefer to be surrounded by parenthesis unless the
-- specific sub-expression is unambiguous (like a single integer variable
-- binding)
expr
  :: Constraints phase
  => GrammarDependencies phase
  -> TypeGrammarDependencies phase
  -> PatternGrammarDependencies phase
  -> Level
  -> Grammar (ExprFor phase)
expr grammarDependencies typeGrammarDependencies patternGrammarDependencies level =
  let ?grammarDependencies        = grammarDependencies
      ?typeGrammarDependencies    = typeGrammarDependencies
      ?patternGrammarDependencies = patternGrammarDependencies
   in exprI level

exprI
  :: forall phase
   . ( Implicits phase
     , Constraints phase
     )
  => Level
  -> Grammar (ExprFor phase)
exprI = level unambiguousExprI ambiguousExprI
  where
    unambiguousExprI :: [Grammar (ExprFor phase)]
    unambiguousExprI =
      [ bindingExpr
      , contentBindingExpr
      , exprExtensionExpr
      ]

    ambiguousExprI :: [Grammar (ExprFor phase)]
    ambiguousExprI =
      [ lamExpr
      , bigLamExpr
      , appExpr
      , bigAppExpr
      , sumExpr
      , productExpr
      , unionExpr
      , caseAnalysisExpr
      ]


{- Misc -}

voidG :: Grammar Void
voidG = rpure void -- This wont work?


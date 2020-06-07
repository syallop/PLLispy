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
Module      : PLLispy.Pattern
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Pattern with a lisp-like syntax.
-}
module PLLispy.Pattern
  (
  -- * Concrete patterns
  -- Grammar for a lispy pattern that depends upon grammars for things such as
  -- bindings, etc.
    pattern
  , PatternGrammarDependencies (..)
  , defaultPatternGrammarDependencies

  -- * Pattern constructors
  -- Grammars for alternatives in a general 'PatternFor phase' ast that defer to
  -- implicit dependent grammars where necessary.
  , patternI
  , sumPattern
  , productPattern
  , unionPattern
  , bindingPattern
  , bindPattern
  , patternExtensionPattern

  -- * Pattern extension Grammars
  --
  -- Grammars for specific extensions to the Pattern ast itself.
  , commentedPattern
  )
  where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..),uncons)
import Data.Text (Text)
import qualified Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import PLGrammar
import Reversible
import Reversible.Iso

import PLLispy.Kind
import PLLispy.Type
import PLLispy.Name
import PLLispy.Level
import PLLispy.Pattern.Iso
import PLLispy.Pattern.Dep
import PLLispy.Type

import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.Pattern
import PL.Kind
import PL.FixPhase
import PL.TyVar
import PL.HashStore
import PL.Name
import PL.Type
import PL.Var

defaultPatternGrammarDependencies
  :: ( PatternConstraints phase

     , Var       ~ BindingFor phase
     , TyVar     ~ TypeBindingFor phase
     , ShortHash ~ TypeContentBindingFor phase

     , NoExt ~ SumPatternExtension phase
     , NoExt ~ ProductPatternExtension phase
     , NoExt ~ UnionPatternExtension phase
     , NoExt ~ BindingPatternExtension phase
     , NoExt ~ BindExtension phase

     , NoExt ~ NamedExtension phase
     , NoExt ~ ArrowExtension phase
     , NoExt ~ SumTExtension phase
     , NoExt ~ ProductTExtension phase
     , NoExt ~ UnionTExtension phase
     , NoExt ~ BigArrowExtension phase
     , NoExt ~ TypeLamExtension phase
     , NoExt ~ TypeAppExtension phase
     , NoExt ~ TypeBindingExtension phase
     , NoExt ~ TypeContentBindingExtension phase

     , (Commented (PatternFor phase)) ~ PatternExtension phase
     , (Commented (TypeFor phase)) ~ TypeExtension phase
     )
  => PatternGrammarDependencies phase
defaultPatternGrammarDependencies = PatternGrammarDependencies
  { _patternBindingFor = var

  , _sumPatternGrammarExtension     = noExtG
  , _productPatternGrammarExtension = noExtG
  , _unionPatternGrammarExtension   = noExtG
  , _bindingPatternGrammarExtension = noExtG
  , _bindGrammarExtension           = noExtG

  , _patternGrammarExtension = commentedPattern defaultPatternGrammarDependencies defaultTypeGrammarDependencies
  }


-- | The Sum pattern constructor takes the form:
--
-- PLUS natural PATTERN
sumPattern
  :: ( PatternImplicits phase
     , PatternConstraints phase
     )
  => Grammar (PatternFor phase)
sumPattern =
  plus */
  (sumPatternIso \$/ sumPatternExtension
                 \*/ (spaceAllowed */ natural)
                 \*/ spacePreferred */ sub patternI)

-- | The Product pattern constructor takes the form:
--
-- STAR PATTERN*
productPattern
  :: ( PatternImplicits phase
     , PatternConstraints phase
     )
  => Grammar (PatternFor phase)
productPattern =
  star */
  (productPatternIso \$/ productPatternExtension
                     \*/ (spaceAllowed */ sepBy spacePreferred (sub patternI)))

-- | The Union pattern constructor takes the form:
--
-- UNION TYPE PATTERN
unionPattern
  :: ( PatternImplicits phase
     , PatternConstraints phase
     )
  => Grammar (PatternFor phase)
unionPattern =
  union */
  (unionPatternIso \$/ unionPatternExtension
                   \*/ (spaceAllowed   */ sub typI)
                   \*/ (spacePreferred */ sub patternI))

-- | The binding pattern constructor is some form of reference to a value bound
-- by an expression abstraction. It is likely to be an index of a name.
--
-- It defers to the implicit binding grammar.
bindingPattern
  :: ( PatternImplicits phase
     , PatternConstraints phase
     )
  => Grammar (PatternFor phase)
bindingPattern =
  bindingPatternIso \$/ bindingPatternExtension
                    \*/ patternBinding

-- | The Bind pattern constructor takes the form:
--
-- ?
bindPattern
  :: ( PatternImplicits phase
     , PatternConstraints phase
     )
  => Grammar (PatternFor phase)
bindPattern =
  question */
  (bindIso \$/ bindExtension)

-- | Defer to the implicit Grammar for pattern extensions.
patternExtensionPattern
  :: ( PatternImplicits phase
     , PatternConstraints phase
     )
  => Grammar (PatternFor phase)
patternExtensionPattern =
  patternExtensionIso \$/ patternExtension

pattern
  :: PatternConstraints phase
  => PatternGrammarDependencies phase
  -> TypeGrammarDependencies phase
  -> Level
  -> Grammar (PatternFor phase)
pattern patternGrammarDependencies typeGrammarDependencies level =
  let ?patternGrammarDependencies = patternGrammarDependencies
      ?typeGrammarDependencies    = typeGrammarDependencies
   in patternI level

patternI
  :: forall phase
   . ( PatternImplicits phase
     , PatternConstraints phase
     )
  => Level
  -> Grammar (PatternFor phase)
patternI = level unambiguous ambiguous
  where
    unambiguous :: [Grammar (PatternFor phase)]
    unambiguous =
      [ bindPattern
      , bindingPattern
      , patternExtensionPattern
      ]

    ambiguous :: [Grammar (PatternFor phase)]
    ambiguous =
      [ sumPattern
      , productPattern
      , unionPattern
      ]

-- A Commented Pattern
commentedPattern
  :: PatternConstraints phase
  => PatternGrammarDependencies phase
  -> TypeGrammarDependencies phase
  -> Grammar (Commented (PatternFor phase))
commentedPattern pDep tDep =
  charIs '"' */
  (commentedPatternIso \$/ (commentText \* charIs '"')
                       \*/ (spaceAllowed */ sub (pattern pDep tDep))
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

{- Misc -}

noExtG :: Grammar NoExt
noExtG = rpure noExt -- This wont work?


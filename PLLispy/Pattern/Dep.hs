{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , FlexibleContexts
  #-}
{-|
Module      : PLLispy.Pattern.Dep
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Encapsulate the dependencies required to construct a Grammar for patterns.

PatternGrammarDependencies contains the dependencies in a big record.

Threading many constant parameters through recursive Grammars can be noisy and so it
_may_ be preferable to make use of implicit parameters. Helper types and
functions are provided for this purpose.

E.G. Instead of writing:

explicitGrammar :: GrammarDependencies phase -> Grammar (PatternFor phase)
explicitGrammar d = iso
 \$/ _patternBidingFor d
 \*/ _sumPatternExtensionFor d
 \*/ _productPatternExtensionFor d
 \*/ g d

You may write:

implicitGrammar :: Implicits phase => Grammar (PatternFor phase)
implicitGrammar =  iso
 \$/ patternBinding
 \*/ sumPatternExtension
 \*/ productPatternExtension
 \*/ g

This can be thought of as working like a typeclass, but where the instance is manually supplied by
a call to 'explicitly' (or by binding let ?patternGrammarDependencies = ... in ...).
This is more flexible than using a typeclass which would require newtype wrapping/
phantom type parameters or code duplication.

In a previous iteration each parameter was passed individually and so this was
a great improvement.
With dependencies captured in a single record the benefit is now arguably not
worth the weight of using the uncommon implicit parameter functionality.
-}
module PLLispy.Pattern.Dep
  ( PatternGrammarDependencies (..)

  , PatternImplicits
  , patternExplicitly
  , patternImplicitly

  , patternBinding
  , sumPatternExtension
  , productPatternExtension
  , unionPatternExtension
  , bindingPatternExtension
  , bindExtension
  , patternExtension

  , PatternConstraints
  )
  where

import PLLispy.Type.Dep

import PL.Pattern
import PL.Expr
import PL.Type

import PLGrammar

-- | Dependencies required to parse a 'PatternFor phase'.
data PatternGrammarDependencies phase = PatternGrammarDependencies
  { _patternBindingFor :: Grammar (BindingFor phase)

  , _sumPatternGrammarExtension     :: Grammar (SumPatternExtension phase)
  , _productPatternGrammarExtension :: Grammar (ProductPatternExtension phase)
  , _unionPatternGrammarExtension   :: Grammar (UnionPatternExtension phase)
  , _bindingPatternGrammarExtension :: Grammar (BindingPatternExtension phase)
  , _bindGrammarExtension           :: Grammar (BindExtension phase)

  , _patternGrammarExtension :: Grammar (PatternExtension phase)
  }

type PatternConstraints phase =
  ( Show (PatternFor phase)
  , Eq (PatternFor phase)
  , Ord (PatternFor phase)

  , Show (SumPatternExtension phase)
  , Show (ProductPatternExtension phase)
  , Show (UnionPatternExtension phase)
  , Show (BindingPatternExtension phase)
  , Show (BindExtension phase)
  , Show (PatternExtension phase)

  , Ord (SumPatternExtension phase)
  , Ord (ProductPatternExtension phase)
  , Ord (UnionPatternExtension phase)
  , Ord (BindingPatternExtension phase)
  , Ord (BindExtension phase)
  , Ord (PatternExtension phase)

  , Show (BindingFor phase)
  , Show (TypeBindingFor phase)
  , TypeConstraints phase

  , Show (TypeFor phase)
  , Ord (TypeFor phase)
  )


-- | Bind PatternGrammarDependencies as an implicit parameter.
type PatternImplicits phase =
  ( ?patternGrammarDependencies :: PatternGrammarDependencies phase
  , TypeImplicits phase
  )

-- | Bind dependency Grammars inside a thing.
patternExplicitly
  :: PatternGrammarDependencies phase
  -> a
  -> a
patternExplicitly patternGrammarDependencies a =
  let ?patternGrammarDependencies = patternGrammarDependencies
   in a

-- | Pass PatternGrammarDependencies to a function from an implicit parameter.
patternImplicitly
  :: PatternImplicits phase
  => (PatternGrammarDependencies phase -> a)
  -> a
patternImplicitly f =
  let x = ?patternGrammarDependencies
   in f x

-- | Defer to implicit Grammar for pattern bindings.
patternBinding :: PatternImplicits phase => Grammar (BindingFor phase)
patternBinding = patternImplicitly _patternBindingFor

-- | Defer to the implicit Grammar for pattern extensions.
patternExtension
  :: PatternImplicits phase
  => Grammar (PatternExtension phase)
patternExtension = patternImplicitly _patternGrammarExtension

-- | Defer to the implicit Grammar for sum pattern extensions.
sumPatternExtension
  :: PatternImplicits phase
  => Grammar (SumPatternExtension phase)
sumPatternExtension = patternImplicitly _sumPatternGrammarExtension

-- | Defer to the implicit Grammar for product pattern extensions.
productPatternExtension
  :: PatternImplicits phase
  => Grammar (ProductPatternExtension phase)
productPatternExtension = patternImplicitly _productPatternGrammarExtension

-- | Defer to the implicit Grammar for union pattern extensions.
unionPatternExtension
  :: PatternImplicits phase
  => Grammar (UnionPatternExtension phase)
unionPatternExtension = patternImplicitly _unionPatternGrammarExtension

-- | Defer to the implicit Grammar for binding pattern extensions.
bindingPatternExtension
  :: PatternImplicits phase
  => Grammar (BindingPatternExtension phase)
bindingPatternExtension = patternImplicitly _bindingPatternGrammarExtension

-- | Defer to the implicit Grammar for bind pattern extensions.
bindExtension
  :: PatternImplicits phase
  => Grammar (BindExtension phase)
bindExtension = patternImplicitly _bindGrammarExtension


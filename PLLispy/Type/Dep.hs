{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , FlexibleContexts
  , MonoLocalBinds
  #-}
{-|
Module      : PLLispy.Type.Dep
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Encapsulate the dependencies required to construct a Grammar for types.

TypeGrammarDependencies contains the dependencies in a big record.

Threading many constant parameters through recursive Grammars can be noisy and so it
_may_ be preferable to make use of implicit parameters. Helper types and
functions are provided for this purpose.

E.G. Instead of writing:

explicitGrammar :: TypeGrammarDependencies phase -> Grammar (TypeFor phase)
explicitGrammar d = iso
 \$/ _typeBinding d
 \*/ _typeContentBinding d
 \*/ _sumTExtension d
 \*/ _productTExtension d
 \*/ g d

You may write:

implicitGrammar :: Implicits phase => Grammar (TypeFor phase)
implicitGrammar =  iso
 \$/ typeBinding
 \*/ typeContentBinding
 \*/ sumTExtension
 \*/ productTExtension
 \*/ g

This can be thought of as working like a typeclass, but where the instance is manually supplied by
a call to 'typeExplicitly' (or by binding let ?typeGrammarDependencies = ... in ...).
This is more flexible than using a typeclass which would require newtype wrapping/
phantom type parameters or code duplication.

In a previous iteration each parameter was passed individually and so this was
a great improvement.
With dependencies captured in a single record the benefit is now arguably not
worth the weight of using the uncommon implicit parameter functionality.
-}
module PLLispy.Type.Dep
  ( TypeGrammarDependencies (..)

  , TypeImplicits
  , typeExplicitly
  , typeImplicitly

  , typeBinding
  , typeContentBinding
  , typeExtension
  , namedExtension
  , arrowExtension
  , sumTExtension
  , productTExtension
  , unionTExtension
  , bigArrowExtension
  , typeLamExtension
  , typeAppExtension
  , typeBindingExtension
  , typeContentBindingExtension

  , TypeConstraints
  )
  where

import PL.Type
import PLGrammar

-- | Dependencies required to parse a 'TypeFor phase'.
data TypeGrammarDependencies phase = TypeGrammarDependencies
  { _typeBindingFor        :: Grammar (TypeBindingFor phase)
  , _typeContentBindingFor :: Grammar (TypeContentBindingFor phase)

  , _namedGrammarExtension              :: Grammar (NamedExtension phase)
  , _arrowGrammarExtension              :: Grammar (ArrowExtension phase)
  , _sumTGrammarExtension               :: Grammar (SumTExtension phase)
  , _productTGrammarExtension           :: Grammar (ProductTExtension phase)
  , _unionTGrammarExtension             :: Grammar (UnionTExtension phase)
  , _bigArrowGrammarExtension           :: Grammar (BigArrowExtension phase)
  , _typeLamGrammarExtension            :: Grammar (TypeLamExtension phase)
  , _typeAppGrammarExtension            :: Grammar (TypeAppExtension phase)
  , _typeBindingGrammarExtension        :: Grammar (TypeBindingExtension phase)
  , _typeContentBindingGrammarExtension :: Grammar (TypeContentBindingExtension phase)

  , _typeGrammarExtension :: Grammar (TypeExtension phase)
  }

type TypeConstraints phase =
  ( Show (TypeFor phase)
  , Eq (TypeFor phase)
  , Ord (TypeFor phase)

  , Show (NamedExtension phase)
  , Show (ArrowExtension phase)
  , Show (SumTExtension phase)
  , Show (ProductTExtension phase)
  , Show (UnionTExtension phase)
  , Show (BigArrowExtension phase)
  , Show (TypeLamExtension phase)
  , Show (TypeAppExtension phase)
  , Show (TypeBindingExtension phase)
  , Show (TypeContentBindingExtension phase)
  , Show (TypeExtension phase)

  , Show (TypeBindingFor phase)
  , Show (TypeContentBindingFor phase)

  , Ord (NamedExtension phase)
  , Ord (ArrowExtension phase)
  , Ord (SumTExtension phase)
  , Ord (ProductTExtension phase)
  , Ord (UnionTExtension phase)
  , Ord (BigArrowExtension phase)
  , Ord (TypeLamExtension phase)
  , Ord (TypeAppExtension phase)
  , Ord (TypeBindingExtension phase)
  , Ord (TypeContentBindingExtension phase)
  , Ord (TypeExtension phase)
  )


-- | Bind TypeGrammarDependencies as an implicit parameter.
type TypeImplicits phase = ?typeGrammarDependencies :: TypeGrammarDependencies phase

-- | Bind dependency Grammars inside a thing.
typeExplicitly
  :: TypeGrammarDependencies phase
  -> a
  -> a
typeExplicitly typeGrammarDependencies a =
  let ?typeGrammarDependencies = typeGrammarDependencies
   in a

-- | Pass TypeGrammarDependencies to a function from an implicit parameter.
typeImplicitly
  :: TypeImplicits phase
  => (TypeGrammarDependencies phase -> a)
  -> a
typeImplicitly f =
  let x = ?typeGrammarDependencies
   in f x

-- | Defer to the implicit Grammar for type bindings.
typeBinding :: TypeImplicits phase => Grammar (TypeBindingFor phase)
typeBinding = typeImplicitly _typeBindingFor

-- | Defer to the implicit Grammar for type content bindings.
typeContentBinding :: TypeImplicits phase => Grammar (TypeContentBindingFor phase)
typeContentBinding = typeImplicitly _typeContentBindingFor

-- | Defer to the implicit Grammar for type extensions.
typeExtension
  :: TypeImplicits phase
  => Grammar (TypeExtension phase)
typeExtension = typeImplicitly _typeGrammarExtension

-- | Defer to the implicit Grammar for type name extensions.
namedExtension
  :: TypeImplicits phase
  => Grammar (NamedExtension phase)
namedExtension = typeImplicitly _namedGrammarExtension

-- | Defer to the implicit Grammar for type arrow extensions.
arrowExtension
  :: TypeImplicits phase
  => Grammar (ArrowExtension phase)
arrowExtension = typeImplicitly _arrowGrammarExtension

-- | Defer to the implicit Grammar for type sum extensions.
sumTExtension
  :: TypeImplicits phase
  => Grammar (SumTExtension phase)
sumTExtension = typeImplicitly _sumTGrammarExtension

-- | Defer to the implicit Grammar for type product extensions.
productTExtension
  :: TypeImplicits phase
  => Grammar (ProductTExtension phase)
productTExtension = typeImplicitly _productTGrammarExtension

-- | Defer to the implicit Grammar for type union extensions.
unionTExtension
  :: TypeImplicits phase
  => Grammar (UnionTExtension phase)
unionTExtension = typeImplicitly _unionTGrammarExtension

-- | Defer to the implicit Grammar for type big arrow extensions.
bigArrowExtension
  :: TypeImplicits phase
  => Grammar (BigArrowExtension phase)
bigArrowExtension = typeImplicitly _bigArrowGrammarExtension

-- | Defer to the implicit Grammar for type lambda extensions.
typeLamExtension
  :: TypeImplicits phase
  => Grammar (TypeLamExtension phase)
typeLamExtension = typeImplicitly _typeLamGrammarExtension

-- | Defer to the implicit Grammar for type application extensions.
typeAppExtension
  :: TypeImplicits phase
  => Grammar (TypeAppExtension phase)
typeAppExtension = typeImplicitly _typeAppGrammarExtension

-- | Defer to the implicit Grammar for type binding extensions.
typeBindingExtension
  :: TypeImplicits phase
  => Grammar (TypeBindingExtension phase)
typeBindingExtension = typeImplicitly _typeBindingGrammarExtension

-- | Defer to the implicit Grammar for type content binding extensions.
typeContentBindingExtension
  :: TypeImplicits phase
  => Grammar (TypeContentBindingExtension phase)
typeContentBindingExtension = typeImplicitly _typeContentBindingGrammarExtension


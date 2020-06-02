{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , FlexibleContexts
  #-}
{-|
Module      : PLLispy.Expr.Dep
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Encapsulate the dependencies required to construct a Grammar for expressions.

GrammarDependencies contains the dependencies in a big record.

Threading many constant parameters through recursive Grammars can be noisy and so it
_may_ be preferable to make use of implicit parameters. Helper types and
functions are provided for this purpose.

E.G. Instead of writing:

explicitGrammar :: GrammarDependencies phase -> Grammar (ExprFor phase)
explicitGrammar d = iso
 \$/ _bidingFor d
 \*/ _contentBindingFor d
 \*/ _abstractionFor d
 \*/ _typeBindingFor d
 \*/ _typeContentBindingFor d
 \*/ _exprExtensionFor d
 \*/ g d

You may write:

implicitGrammar :: Implicits phase => Grammar (ExprFor phase)
implicitGrammar =  iso
 \$/ binding
 \*/ contentBinding
 \*/ abstraction
 \*/ typeBinding
 \*/ typeContentBinding
 \*/ exprExtension
 \*/ g

This can be thought of as working like a typeclass, but where the instance is manually supplied by
a call to 'explicitly' (or by binding let ?grammarDependencies = ... in ...).
This is more flexible than using a typeclass which would require newtype wrapping/
phantom type parameters or code duplication.

In a previous iteration each parameter was passed individually and so this was
a great improvement.
With dependencies captured in a single record the benefit is now arguably not
worth the weight of using the uncommon implicit parameter functionality.
-}
module PLLispy.Expr.Dep
  ( GrammarDependencies (..)

  , Implicits
  , explicitly
  , implicitly

  , binding
  , contentBinding
  , abstraction
  , exprTypeBinding
  , exprTypeContentBinding

  , lamExtension
  , appExtension
  , bindingExtension
  , contentBindingExtension
  , caseAnalysisExtension
  , sumExtension
  , productExtension
  , unionExtension
  , bigLamExtension
  , bigAppExtension

  , exprExtension

  , Constraints
  )
  where

import PLLispy.Type.Dep
import PLLispy.Pattern.Dep

import PL.Expr hiding (appise,lamise)
import PL.Type
import PLGrammar

-- | Dependencies required to parse a 'ExprFor phase'.
data GrammarDependencies phase = GrammarDependencies
  { _bindingFor                :: Grammar (BindingFor phase)
  , _contentBindingFor         :: Grammar (ContentBindingFor phase)
  , _abstractionFor            :: Grammar (AbstractionFor phase)
  , _exprTypeBindingFor        :: Grammar (TypeBindingFor phase)
  , _exprTypeContentBindingFor :: Grammar (TypeContentBindingFor phase)

  -- Extensions to each of the expr constructors
  , _lamGrammarExtension            :: Grammar (LamExtension phase)
  , _appGrammarExtension            :: Grammar (AppExtension phase)
  , _bindingGrammarExtension        :: Grammar (BindingExtension phase)
  , _contentBindingGrammarExtension :: Grammar (ContentBindingExtension phase)
  , _caseAnalysisGrammarExtension   :: Grammar (CaseAnalysisExtension phase)
  , _sumGrammarExtension            :: Grammar (SumExtension phase)
  , _productGrammarExtension        :: Grammar (ProductExtension phase)
  , _unionGrammarExtension          :: Grammar (UnionExtension phase)
  , _bigLamGrammarExtension         :: Grammar (BigLamExtension phase)
  , _bigAppGrammarExtension         :: Grammar (BigAppExtension phase)

  -- Extensions to the expr ast itself
  , _exprGrammarExtension           :: Grammar (ExprExtension phase)
  }

type Constraints phase =
  ( Show (ExprFor phase)
  , Eq (ExprFor phase)
  , Ord (ExprFor phase)

  , Show (LamExtension phase)
  , Show (AppExtension phase)
  , Show (BindingExtension phase)
  , Show (ContentBindingExtension phase)
  , Show (CaseAnalysisExtension phase)
  , Show (SumExtension phase)
  , Show (ProductExtension phase)
  , Show (UnionExtension phase)
  , Show (BigLamExtension phase)
  , Show (BigAppExtension phase)
  , Show (ExprExtension phase)

  , Show (AbstractionFor phase)
  , Show (BindingFor phase)
  , Show (ContentBindingFor phase)

  , TypeConstraints phase
  , PatternConstraints phase
  )


-- | Bind GrammarDependencies as an implicit parameter.
type Implicits phase =
  ( ?grammarDependencies :: GrammarDependencies phase
  , TypeImplicits phase
  , PatternImplicits phase
  )

-- | Bind dependency Grammars inside a thing.
explicitly
  :: GrammarDependencies phase
  -> a
  -> a
explicitly grammarDependencies a =
  let ?grammarDependencies = grammarDependencies
   in a

-- | Pass GrammarDependencies to a function from an implicit parameter.
implicitly
  :: Implicits phase
  => (GrammarDependencies phase -> a)
  -> a
implicitly f = f ?grammarDependencies

-- | Defer to the implicit Grammar for expr bindings.
binding :: Implicits phase => Grammar (BindingFor phase)
binding = implicitly _bindingFor

-- | Defer to the implicit Grammar for expr content bindings.
contentBinding :: Implicits phase => Grammar (ContentBindingFor phase)
contentBinding = implicitly _contentBindingFor

-- | Defer to the implicit Grammar for expr abstractions.
abstraction :: Implicits phase => Grammar (AbstractionFor phase)
abstraction = implicitly _abstractionFor

-- | Defer to the implicit Grammar for type bindings.
exprTypeBinding :: Implicits phase => Grammar (TypeBindingFor phase)
exprTypeBinding = implicitly _exprTypeBindingFor

-- | Defer to the implicit Grammar for type content bindings.
exprTypeContentBinding :: Implicits phase => Grammar (TypeContentBindingFor phase)
exprTypeContentBinding = implicitly _exprTypeContentBindingFor

-- | Defer to the implicit Grammar for lambda extensions.
lamExtension :: Implicits phase => Grammar (LamExtension phase)
lamExtension = implicitly _lamGrammarExtension

-- | Defer to the implicit Grammar for app extensions.
appExtension :: Implicits phase => Grammar (AppExtension phase)
appExtension = implicitly _appGrammarExtension

-- | Defer to the implicit Grammar for binding extensions.
bindingExtension :: Implicits phase => Grammar (BindingExtension phase)
bindingExtension = implicitly _bindingGrammarExtension

-- | Defer to the implicit Grammar for content binding extensions.
contentBindingExtension :: Implicits phase => Grammar (ContentBindingExtension phase)
contentBindingExtension = implicitly _contentBindingGrammarExtension

-- | Defer to the implicit Grammar for case analysis extensions.
caseAnalysisExtension :: Implicits phase => Grammar (CaseAnalysisExtension phase)
caseAnalysisExtension = implicitly _caseAnalysisGrammarExtension

-- | Defer to the implicit Grammar for sum extensions.
sumExtension :: Implicits phase => Grammar (SumExtension phase)
sumExtension = implicitly _sumGrammarExtension

-- | Defer to the implicit Grammar for product extensions.
productExtension :: Implicits phase => Grammar (ProductExtension phase)
productExtension = implicitly _productGrammarExtension

-- | Defer to the implicit Grammar for union extensions.
unionExtension :: Implicits phase => Grammar (UnionExtension phase)
unionExtension = implicitly _unionGrammarExtension

-- | Defer to the implicit Grammar for big lam extensions.
bigLamExtension :: Implicits phase => Grammar (BigLamExtension phase)
bigLamExtension = implicitly _bigLamGrammarExtension

-- | Defer to the implicit Grammar for big app extensions.
bigAppExtension :: Implicits phase => Grammar (BigAppExtension phase)
bigAppExtension = implicitly _bigAppGrammarExtension

-- | Defer to the implicit Grammar for expr extensions.
exprExtension
  :: Implicits phase
  => Grammar (ExprExtension phase)
exprExtension = implicitly _exprGrammarExtension


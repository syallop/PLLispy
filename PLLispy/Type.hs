{-# LANGUAGE
    ScopedTypeVariables
  , ImplicitParams
  , OverloadedStrings
  , FlexibleContexts
  , MultiWayIf
  , GADTs
  , ConstraintKinds
  #-}
{-|
Module      : PLLispy.Type
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Type with a lisp-like syntax.
-}
module PLLispy.Type
  (
  -- * Concrete types
  --
  -- Grammar for a lispy type that depends upon grammars for things such as type
  -- bindings, etc.
    typ
  , TypeGrammarDependencies (..)
  , defaultTypeGrammarDependencies

  -- * Type constructors
  -- Grammars for alternatives in a general 'TypeFor phase' ast that defer to
  -- implicit dependent grammars where necessary.
  , typI
  , arrowTyp
  , typeBindingTyp
  , typeContentBindingTyp
  , namedTyp
  , sumTyp
  , productTyp
  , unionTyp
  , bigArrowTyp
  , typeLamTyp
  , typeAppTyp
  , typeExtensionTyp

  -- * Type extension Grammars
  --
  -- Grammars for specific extensions to the Type ast itself.
  , commentedTyp

  -- * Grammar Dependency options
  --
  -- Grammars that are likely to be usd to instantiate the dependencies

  -- ** Type bindings
  , tyVar

  -- ** Expr abstractions
  , kind

  -- ** TypeContentBindings at Type level
  , shortHash
  , contentName
  )
  where

import Prelude hiding (takeWhile)

import Control.Applicative
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as Text

import PLLispy.Type.Iso
import PLLispy.Type.Dep
import PLLispy.Kind
import PLLispy.Level
import PLLispy.Name

import PLGrammar
import Reversible.Iso
import Reversible

import PL.Commented
import PL.Expr
import PL.HashStore
import PL.FixPhase
import PL.Kind
import PL.Name
import PL.TyVar
import PL.Type hiding (arrowise)

defaultTypeGrammarDependencies
  :: ( TypeConstraints phase

     , TyVar       ~ TypeBindingFor phase
     , ShortHash   ~ TypeContentBindingFor phase

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

     , (Commented (TypeFor phase)) ~ TypeExtension phase
     )
  => TypeGrammarDependencies phase
defaultTypeGrammarDependencies = TypeGrammarDependencies
  { _typeBindingFor        = tyVar
  , _typeContentBindingFor = shortHash

  , _namedGrammarExtension              = noExtG
  , _arrowGrammarExtension              = noExtG
  , _sumTGrammarExtension               = noExtG
  , _productTGrammarExtension           = noExtG
  , _unionTGrammarExtension             = noExtG
  , _bigArrowGrammarExtension           = noExtG
  , _typeLamGrammarExtension            = noExtG
  , _typeAppGrammarExtension            = noExtG
  , _typeBindingGrammarExtension        = noExtG
  , _typeContentBindingGrammarExtension = noExtG

  , _typeGrammarExtension = commentedTyp defaultTypeGrammarDependencies
  }


-- | The Arrow constructor takes the form:
--
-- ARROW TYPE TYPE
arrowTyp
  :: forall phase
   . ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
arrowTyp =
  arrow */ (arrowIso \$/ arrowExtension
                     \*/ (spaceAllowed */ sub typI)
                     \*/ (spacePreferred */ sub typI))

-- | The type binding constructor is some form of reference to a value bound by
-- a type abstraction. It is likely to be an index of a name.
--
-- It defers to the implicit type binding grammar.
typeBindingTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
typeBindingTyp =
  typeBindingIso \$/ typeBindingExtension
                 \*/ typeBinding

-- | The type content binding constructor is a name which references a type by
-- it's content.
--
-- It defers to the implicit type content binding grammar.
typeContentBindingTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
typeContentBindingTyp =
  typeContentBindingIso \$/ typeContentBindingExtension
                        \*/ typeContentBinding

-- | The Named type constructor takes the form:
--
-- UPPER CHARACTERS*
namedTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
namedTyp =
  namedIso \$/ namedExtension
           \*/ typeNameTyp

-- | The Sum type constructor takes the form:
--
-- PLUS TYPE+
sumTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
sumTyp =
  plus */
  (sumTIso \$/ sumTExtension
           \*/ (spaceAllowed */ (sepBy1 spacePreferred $ sub typI)))

-- | The Product type constructor takes the form:
--
-- PRODUCT TYPE*
productTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
productTyp =
  star */
  (productTIso \$/ productTExtension
               \*/ (spaceAllowed */ sepBy spacePreferred (sub typI)))

-- | The Union type constructor takes the form:
--
-- UNION TYPE*
unionTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
unionTyp =
  union */
  (unionTIso \$/ unionTExtension
             \*/ (spaceAllowed */ (setIso \$/ sepBy spacePreferred (sub typI))))

-- | The Big Arrow type constructor takes the form:
--
-- BIGARROW kind TYPE
bigArrowTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
bigArrowTyp =
  bigArrow */ (bigArrowIso \$/ bigArrowExtension
                           \*/ (spaceAllowed */ parensKind)
                           \*/ (spacePreferred */ sub typI))
  where
    -- TODO:
    -- - Better ascii symbol
    -- - Unicode symbol
    -- If Lambda       '\'  has type '->'
    -- Then BigLambda  '/\' has type '/->'
    bigArrow = textIs "/->"

-- | The Type Lambda type constructor takes the form:
--
-- BIGLAMBDA kind TYPE
typeLamTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
typeLamTyp =
  bigLambda */ (typeLamIso \$/ typeLamExtension
                           \*/ (spaceAllowed */ (parensPreferred kindAbs))
                           \*/ (spacePreferred */ sub typI))

-- | The Type App type constructor takes the form:
--
-- BIGAPP TYPE TYPE
typeAppTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
typeAppTyp =
  bigApp */ (typeAppIso \$/ typeAppExtension
                        \*/ (spaceAllowed */ sub typI)
                        \*/ (spacePreferred */ sub typI))
  where
    bigApp = textIs "/@"

-- | Defer to the implicit Grammar for type extensions.
typeExtensionTyp
  :: ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Grammar (TypeFor phase)
typeExtensionTyp =
  typeExtensionIso \$/ typeExtension

-- | A Commented Type as the type extension constructor takes the form:
--
-- '"' COMMENT '"' TYPE
commentedTyp
  :: TypeConstraints phase
  => TypeGrammarDependencies phase
  -> Grammar (Commented (TypeFor phase))
commentedTyp tDep =
  charIs '"' */
  (commentedTypeIso \$/ (commentText \* charIs '"')
                    \*/ (spaceAllowed */ sub (typ tDep))
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

-- | tyVar can be used as a types binding.
--
-- It refers to a bound value by counting back to the type lambda which
-- abstracted it. It takes the form of a natural number E.G.:
--
-- ?0,?1,?2,...
tyVar
  :: Grammar TyVar
tyVar =
  charIs '?' */
  (tyVarIso \$/ natural)

-- | Parse a type given grammar dependencies for things such as:
-- - Type bindings (E.G. tyVar)
--
-- and a level signifier.
--
-- Top-level types prefer not to be surrounded by parenthesis.
--
-- Nested sub-types will prefer to be surrounded by parenthesis unless the
-- specific sub-type is unambgiuous (like a single integer type variable
-- binding)
typ
  :: TypeConstraints phase
  => TypeGrammarDependencies phase
  -> Level
  -> Grammar (TypeFor phase)
typ typeGrammarDependencies level =
  let ?typeGrammarDependencies = typeGrammarDependencies
   in typI level

typI
  :: forall phase
   . ( TypeImplicits phase
     , TypeConstraints phase
     )
  => Level
  -> Grammar (TypeFor phase)
typI = level unambiguousTypI ambiguousTypI
  where
    unambiguousTypI :: [Grammar (TypeFor phase)]
    unambiguousTypI =
      [ namedTyp
      , typeContentBindingTyp
      , typeBindingTyp
      , typeExtensionTyp
      --, commentedTyp
      ]

    ambiguousTypI :: [Grammar (TypeFor phase)]
    ambiguousTypI =
      [ typeLamTyp
      , typeAppTyp
      , arrowTyp
      , sumTyp
      , productTyp
      , unionTyp
      , bigArrowTyp
      ]





{- Misc -}

typeNameTyp :: Grammar TypeName
typeNameTyp = typeNameIso \$/ name

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

noExtG :: Grammar NoExt
noExtG = rpure noExt

nonEmptyIso :: Iso [a] (NonEmpty a)
nonEmptyIso = Iso
  {_forwards = \xs -> case xs of
                 []
                   -> Nothing

                 (head:tail)
                   -> Just $ head NE.:| tail
  ,_backwards = Just . NE.toList
  }


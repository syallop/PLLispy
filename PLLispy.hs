{-# LANGUAGE
    RankNTypes
  , OverloadedStrings
  , GADTs
  , FlexibleContexts
  , LambdaCase
  , ScopedTypeVariables
  #-}
{-|
Module      : PLLispy
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL with a lisp-like syntax.

This module:
- Re-exposes more general grammars for defining expressions/ type grammars
- Provides example implementations of transforming arbitrary Grammars into
  parsers/ printers.
- Defines some concrete expression/ type/ pattern grammars.
-}
module PLLispy
  ( module X

  -- * Interpret Grammars
  , toParser
  , toPrinter

  -- * Concrete Lispy Grammars
  --
  -- These Grammars are configured to parse lispy expressions, types and
  -- grammars into concrete phases.
  --
  -- Grammars generalised over the type of phase have a ' suffix.
  --
  -- Custom Grammars can be configured from PLLispy.{Expr,Type,Pattern}.
  , lispyExpr
  , lispyType
  , lispyPattern

  -- * General Lispy Grammars
  , lispyExpr'
  , lispyType'
  , lispyPattern'
  )
  where

-- Lispy
import PLLispy.Case      as X
import PLLispy.Expr      as X
import PLLispy.Kind      as X
import PLLispy.Pattern   as X
import PLLispy.Type      as X
import PLLispy.Level     as X
import PLLispy.Expr.Dep  as X
import PLLispy.Type.Dep  as X
import PLLispy.Pattern.Dep  as X

-- Core PL
import PL.Commented
import PL.Expr
import PL.FixPhase
import PL.Pattern
import PL.TyVar
import PL.Type
import PL.Var

-- Other PL
import PLGrammar
import PLHash.Short
import PLLabel
import PLParser
import PLParser.State
import PLParser.Expected
import PLPrinter
import Reversible
import Reversible.Iso
import qualified PLGrammar as G
import qualified PLParser  as P

-- Other
import qualified Data.Text as Text

import Control.Applicative
import Control.Monad

-- | Parse an expression in the concrete CommentedPhase which is suitable to be
-- read/ written by humans. It allows comments surrounding expressions, types
-- and patterns and uses (potentially ambiguous) short-hashes for
-- content-bindings.
--
-- For more details see lispyExpr' which does not constrain a specific phase.
lispyExpr
  :: Grammar (ExprFor CommentedPhase)
lispyExpr = lispyExpr'

-- | Parse an expression in any phase which is suitable to be read/ written by humans.
-- It allows comments surrounding expressions, types and patterns and uses
-- (potentially ambiguous) short-hashes for content-bindings.
--
-- The CommentedPhase is expected to conform to this Grammar - see lispyExpr.
--
-- - Bindings are debruijn indices counting the number of abstractions away a
--   variable was bound.
--
-- - Abstractions are unnamed and annotate the type/ kind of variable they
--   abstract. I.E. Expression abstractions are Types and type abstractions are
--   Kinds.
--
-- - ContentBindings - which refer to things by their content addressed hash -
--   are - potentially ambiguous - ShortHashes that may omit their algorithm and
--   trailing characters in an attempt to be more human readable.
--
-- - Expressions, types and patterns may be annotated with a comment using
--   quotation marks.
--
-- - Types are lispyTypes and patterns are lispyPatterns
--
-- - There are no other permitted extensions.
--
-- - All other constructors take the form:
--   TOKEN ARGS*
--
--   where:
--   - The TOKEN character identifies the sort of expression/ type/ pattern.
--   - The token is followed by zero or many space separated arguments
--   - Unambiguous things may be surrounded by parenthesis; Ambiguous things
--     must be surrounded by parenthesis.
lispyExpr'
  :: forall phase
   . ( Show (ExprFor phase)
     , Show (TypeFor phase)
     , Show (PatternFor phase)
     , Ord (ExprFor phase)
     , Ord (TypeFor phase)
     , Ord (PatternFor phase)

     , Var             ~ BindingFor phase
     , ShortHash       ~ ContentBindingFor phase
     , (TypeFor phase) ~ AbstractionFor phase
     , TyVar           ~ TypeBindingFor phase
     , ShortHash       ~ TypeContentBindingFor phase

     , NoExt ~ LamExtension phase
     , NoExt ~ AppExtension phase
     , NoExt ~ BindingExtension phase
     , NoExt ~ ContentBindingExtension phase
     , NoExt ~ CaseAnalysisExtension phase
     , NoExt ~ SumExtension phase
     , NoExt ~ ProductExtension phase
     , NoExt ~ UnionExtension phase
     , NoExt ~ BigLamExtension phase
     , NoExt ~ BigAppExtension phase

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

     , (Commented (ExprFor phase)) ~ ExprExtension phase
     , (Commented (TypeFor phase)) ~ TypeExtension phase
     , (Commented (PatternFor phase)) ~ PatternExtension phase
     )
  => Grammar (ExprFor phase)
lispyExpr' = top $ expr exprDeps typeDeps patternDeps
  where
    exprDeps :: GrammarDependencies phase
    exprDeps = defaultGrammarDependencies

    typeDeps :: TypeGrammarDependencies phase
    typeDeps = defaultTypeGrammarDependencies

    patternDeps :: PatternGrammarDependencies phase
    patternDeps = defaultPatternGrammarDependencies

-- | Parse a type in the concrete CommentedPhase which is suitable to be read/
-- written by humans. It allows commentes surrounding types and uses
-- (potentially ambiguous) short-hashes for content-bindings.
--
-- For more details see lispyType' which does not constrain a specific phase.
lispyType :: Grammar (TypeFor CommentedPhase)
lispyType = lispyType'

-- | Parse a type in any phase which is suitable to be read/ written by humans.
-- It allows commentes surrounding types and uses
-- (potentially ambiguous) short-hashes for content-bindings.
--
-- The CommentedPhase is expected to conform to this Grammar - see lispyType.
--
-- - Type Bindings are debruijn indices counting the number of abstractions away a
--   type variable was bound.
--
-- - Abstractions are unnamed and annotate the kind of variable they
--   abstract. I.E. Type abstractions are Kinds.
--
-- - ContentBindings - which refer to things by their content addressed hash -
--   are - potentially ambiguous - ShortHashes that may omit their algorithm and
--   trailing characters in an attempt to be more human readable.
--
-- - Types may be annotated with a comment using quotation marks.
--
-- - There are no other permitted extensions
--
-- - All other constructors take the form:
--   TOKEN ARGS*
--
--   where:
--   - The TOKEN character identifies the sort of type
--   - The token is followed by zero or many space separated arguments
--   - Unambiguous things may be surrounded by parenthesis; Ambiguous things
--     must be surrounded by parenthesis.
lispyType'
  :: forall phase
   . ( Show (TypeFor phase)
     , Ord (TypeFor phase)

     , TyVar     ~ TypeBindingFor phase
     , ShortHash ~ TypeContentBindingFor phase

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
  => Grammar (TypeFor phase)
lispyType' = top $ typ typeDeps
  where
    typeDeps :: TypeGrammarDependencies phase
    typeDeps = defaultTypeGrammarDependencies

-- | Parse a pattern in the concrete CommentedPhase which is suitable to be
-- read/ written by humans. It allows commentes surrounding patters and uses
-- (potentially ambiguous) short-hashes for content-bindings.
--
-- For more details see lispyPattern' which does not constrain a specific phase.
lispyPattern :: Grammar (PatternFor CommentedPhase)
lispyPattern = lispyPattern'

-- | Parse a pattern in any phase which is suitable to be read/ written by
-- humans.
-- It allows comments surrounding patterns and uses (potentially ambiguous)
-- short-hashes for content-bindings.
--
-- The CommentedPhase is expected to conform to this Grammar - see lispyPattern.
--
-- - Bindings are debruijn indices counting the number of abstractions away a
--   variable was bound.
--
-- - ContentBindings - which refer to things by their content addressed hash -
--   are - potentially ambiguous - ShortHashes that may omit their algorithm and
--   trailing characters in an attempt to be more human readable.
--
-- - Types and patterns may be annotated with a comment using
--   quotation marks.
--
-- - Types are lispyTypes
--
-- - There are no other permitted extensions.
--
-- - All other constructors take the form:
--   TOKEN ARGS*
--
--   where:
--   - The TOKEN character identifies the sort of type/ pattern.
--   - The token is followed by zero or many space separated arguments
--   - Unambiguous things may be surrounded by parenthesis; Ambiguous things
--     must be surrounded by parenthesis.
lispyPattern'
  :: forall phase
   . ( Show (PatternFor phase)
     , Ord (PatternFor phase)

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

     , (Commented (TypeFor phase)) ~ TypeExtension phase
     , (Commented (PatternFor phase)) ~ PatternExtension phase
     )
  => Grammar (PatternFor phase)
lispyPattern' = top $ pattern patternDeps typeDeps
  where
    patternDeps :: PatternGrammarDependencies phase
    patternDeps = defaultPatternGrammarDependencies

    typeDeps :: TypeGrammarDependencies phase
    typeDeps = defaultTypeGrammarDependencies

-- | Convert any Grammar to a Parser that accepts it.
toParser :: G.Grammar a -> Parser a
toParser = toParser'
  where
    toParser' :: G.Grammar a -> Parser a
    toParser' (Reversible grammar) = case grammar of
      ReversibleInstr i
        -> case i of
             -- A single character if one is available.
             G.GAnyChar
               -> takeChar

             G.GLabel l g
               -> P.label l . toParser' $ g

             G.GTry g0
               -> P.try . toParser' $ g0

      -- Return the value.
      RPure a
        -> pure a

      -- Fail with no Expectations.
      REmpty
        -> empty

      -- If the left fails, try the right as if no input had been consumed.
      RAlt g0 g1
        -> toParser' g0 <|> toParser' g1

      -- Parse the grammar if the iso succeeds.
      RMap iso ga
        -> rmapParser iso ga

      -- Tuple the result of two successive parsers.
      RAp ga gb
        -> rapParser (toParser' ga) (toParser' gb)


    -- A Parser that accepts that grammar if the Iso also succeeds.
    rmapParser
      :: Show a
      => Iso a b
      -> G.Grammar a
      -> Parser b
    rmapParser iso@(Iso _ _) gr = do
      initialCursor <- cursor <$> state
      a             <- toParser' gr
      case forwards iso a of
        Nothing
          -> let grammarExpectations = expect initialCursor (grammarExpects gr) noExpectations
              in P.label (enhancingLabel "ISO")
               . withState (recordExpectations grammarExpectations)
               . failing
               . grammarExpects
               $ gr

        Just b
          -> pure b

    -- | Tuple the result of two successive parsers.
    rapParser :: Parser a -> Parser b -> Parser (a,b)
    rapParser fa fb = (,) <$> fa <*> fb

-- | A Grammar's parser expected to see:
grammarExpects :: forall a. Show a => Grammar a -> Expected
grammarExpects (Reversible g0) = case g0 of
  ReversibleInstr i
    -> case i of
         -- Expected a single character.
         GAnyChar
           -> ExpectN 1 ExpectAnything

         GLabel l g
           -> ExpectLabel l (grammarExpects g)

         GTry g
           -> ExpectPredicate (enhancingLabel "TRY") . Just $ grammarExpects g

  -- Expected a specific thing.
  RPure a
    -> ExpectText . Text.pack . show $ a

  -- Expected to fail.
  REmpty
    -> ExpectFail

  -- Expected one or the other.
  RAlt l r
    -> ExpectEither (grammarExpects l) (grammarExpects r)

  -- Expects something AND a predicate to succeed.
  -- TODO: Capture this desired predicate?
  RMap (Iso _ _) g1
    -> ExpectPredicate (enhancingLabel "ISO") . Just . grammarExpects $ g1

  -- Expected one thing and then another.
  RAp g1 g2
    -> ExpectThen (grammarExpects g1) (grammarExpects g2)


rmapPrinter :: Iso a b -> Printer a -> Printer b
rmapPrinter iso (Printer p) = Printer $ backwards iso >=> p

-- | Convert any Grammar to a Printer that pretty-prints it.
toPrinter :: Grammar a -> Printer a
toPrinter (Reversible grammar) = case grammar of
  ReversibleInstr i
    -> case i of
         GAnyChar
           -> anyCharPrinter

         GLabel _label g
           -> toPrinter g

         GTry g
           -> toPrinter g

  RPure a
    -> purePrinter a

  REmpty
    -> emptyPrinter

  RAlt g0 g1
    -> altPrinter (toPrinter g0) (toPrinter g1)

  RMap iso ga
    -> rmapPrinter iso (toPrinter ga)

  RAp ga gb
    -> rapPrinter (toPrinter ga) (toPrinter gb)


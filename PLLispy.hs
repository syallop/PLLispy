{-# LANGUAGE RankNTypes, OverloadedStrings, GADTs #-}
{-|
Module      : PLLispy
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL with a lisp-like syntax.
-}
module PLLispy
  ( module X
  , toParser
  , toPrinter
  )
  where

import PLLispy.Case      as X
import PLLispy.Expr      as X
import PLLispy.Kind      as X
import PLLispy.MatchArg  as X
import PLLispy.Type      as X

import PLGrammar
import PLLabel
import PLGrammar.Iso
import PLPrinter
import PLParser
import PLParser.Cursor
import PLLabel

import qualified PLGrammar as G
import qualified PLParser  as P

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map  as Map

import Data.Text (Text)
import Data.Char

import Control.Applicative
import Control.Monad
import Data.Monoid

-- | Convert a Grammar to a Parser that accepts it.
toParser :: G.Grammar a -> Parser a
toParser = toParser' . makePermissive
  where
    toParser' :: G.Grammar a -> Parser a
    toParser' grammar = case grammar of
      -- A single character if one is available.
      G.GAnyChar
        -> takeChar

      -- Return the value.
      G.GPure a
        -> pure a

      -- Fail with no Expectations.
      G.GEmpty
        -> empty

      -- If the left fails, try the right as if no input had been consumed.
      G.GAlt g0 g1
        -> toParser' g0 <|> toParser' g1

      -- Parse the grammar if the iso succeeds.
      G.GIsoMap iso ga
        -> isoMapParser iso ga

      -- | Tuple the result of two successive parsers.
      G.GProductMap ga gb
        -> productMapParser (toParser' ga) (toParser' gb)

      -- Enhance a failing parse with a given Expect label.
      G.GLabel l g
        -> let Parser f = toParser' g
            in Parser $ \cur0 -> case f cur0 of
                 ParseFailure failures cur1
                   -> ParseFailure (map (\(e,c) -> (ExpectLabel l e,c)) failures) cur1
                 s -> s

      G.GTry g0
        -> P.try . toParser' $ g0

    -- A Parser that accepts that grammar if the Iso also succeeds.
    isoMapParser
      :: Show a
      => Iso a b
      -> G.Grammar a
      -> Parser b
    isoMapParser iso@(Iso labels _ _) gr =
      let Parser p = toParser' gr
       in Parser $ \cur0 -> case p cur0 of
            ParseSuccess a cur1
              -> case parseIso iso a of
                   Nothing
                     -> ParseFailure [(ExpectLabel (enhancingLabel . Text.intercalate "." $ labels) $ grammarExpects gr, cur0)] cur1 -- cur1

                   Just b
                     -> ParseSuccess b cur1

            ParseFailure failures cur1
              -> ParseFailure failures cur1



    -- | Tuple the result of two successive parsers.
    productMapParser :: Parser a -> Parser b -> Parser (a,b)
    productMapParser fa fb = (,) <$> fa <*> fb

    -- | A permissive grammar allows:
    -- - Leading whitespace
    -- - Trailing whitespace
    -- - Parenthesis grouping allowing:
    --   - Trailing whitespace after the opening paren
    --   - Leading whitespace before the closing paren
    permissive
      :: Show a
      => Grammar a
      -> Grammar a
    permissive g
      = g
      {-= G.try spaceAllowed G.*/ (g \|/ G.between (lparen G.\* G.try spaceAllowed) g (G.try spaceAllowed G.*/ rparen)) G.\* G.try spaceAllowed-}

    -- | Make every sub-grammar 'permissive'.
    makePermissive
      :: Grammar a
      -> Grammar a
    makePermissive g = case g of
      G.GAnyChar
        -> permissive G.GAnyChar

      -- Return the value.
      G.GPure a
        -> G.GPure a

      -- Fail with no Expectations.
      G.GEmpty
        -> G.GEmpty

      -- If the left fails, try the right as if no input had been consumed.
      G.GAlt g0 g1
        -> G.GAlt (makePermissive g0) (makePermissive g1)

      -- Parse the grammar if the iso succeeds.
      G.GIsoMap iso ga
        -> G.GIsoMap iso (makePermissive ga)

      -- | Tuple the result of two successive parsers.
      G.GProductMap ga gb
        -> G.GProductMap (makePermissive ga) (makePermissive gb)

      -- Enhance a failing parse with a given Expect label.
      G.GLabel l g
        -> G.GLabel l (makePermissive g)

      G.GTry g
        -> G.GTry (makePermissive g)

-- | A Grammar's parser expected to see:
grammarExpects :: forall a. Show a => Grammar a -> Expected
grammarExpects g0 = case g0 of
  -- Expected a single character.
  GAnyChar
    -> ExpectN 1 ExpectAnything

  -- Expected a specific thing.
  GPure a
    -> ExpectOneOf [Text.pack . show $ a]

  -- Expected to fail.
  GEmpty
    -> ExpectOneOf []

  -- Expected one or the other.
  GAlt l r
    -> ExpectEither (grammarExpects l) (grammarExpects r)

  -- Expects something AND a predicate to succeed.
  -- TODO: Capture this desired predicate?
  GIsoMap (Iso labels _ _) g1
    -> ExpectPredicate (enhancingLabel . Text.intercalate "." $ labels) . Just . grammarExpects $ g1

  -- Expected one thing and then another.
  GProductMap g1 g2
    -> ExpectThen (grammarExpects g1) (grammarExpects g2)

  GLabel l g
    -> ExpectLabel l (grammarExpects g)

  GTry g
    -> ExpectPredicate (enhancingLabel "TRY") . Just $ grammarExpects g

isoMapPrinter :: Iso a b -> Printer a -> Printer b
isoMapPrinter iso (Printer p) = Printer $ printIso iso >=> p

toPrinter :: Grammar a -> Printer a
toPrinter grammar = case grammar of
  GAnyChar
    -> anyCharPrinter

  GPure a
    -> purePrinter a

  GEmpty
    -> emptyPrinter

  GAlt g0 g1
    -> altPrinter (toPrinter g0) (toPrinter g1)

  GIsoMap iso ga
    -> isoMapPrinter iso (toPrinter ga)

  GProductMap ga gb
    -> productMapPrinter (toPrinter ga) (toPrinter gb)

  GLabel _label g
    -> toPrinter g

  GTry g
    -> toPrinter g

describeGrammar :: Show a => Grammar a -> Doc
describeGrammar gr = case gr of
  GAnyChar
    -> text "."

  GPure a
    -> mconcat
         [ text "( "
         , text (Text.pack . show $ a)
         , text " )"
         ]
  GEmpty
    -> text "()"

  GAlt g0 g1
    -> mconcat [ text "(| "
               , indent1 $ mconcat $
                 [ lineBreak
                 , describeGrammar g0
                 , lineBreak
                 , describeGrammar g1
                 , lineBreak
                 ]
               , text " |)"
               ]

  GIsoMap (Iso labels _ _) g
    -> mconcat [ text "($ "
               , text $ mconcat labels
               , text " $)"
               ]

  GProductMap g0 g1
    -> mconcat [ text "(& "
               , describeGrammar g0
               , text " "
               , describeGrammar g1
               , text " &)"
               ]

  -- Given a descriptive label, we can stop describing further.
  GLabel (Label lTxt Descriptive) g
    -> mconcat [ text "labeled:"
               , text lTxt
               ]

  -- Given an enhancing label, we continue to describe.
  GLabel (Label lTxt Enhancing) g
    -> mconcat [ text "("
               , text lTxt
               , describeGrammar g
               , text lTxt
               , text " )"
               ]

  GTry g0
    -> mconcat [ text " (T "
               , describeGrammar g0
               , text " T) "
               ]

showExpectedDoc :: Expected -> Doc
showExpectedDoc = bulleted
                . flattenExpectedDoc

-- Returns alternatives
flattenExpectedDoc :: Expected -> [Doc]
flattenExpectedDoc e = List.nub $ case e of
  ExpectEither es0 es1
    -> flattenExpectedDoc es0 <> flattenExpectedDoc es1

  ExpectOneOf ts
    -> let oneOf = map text ts
        in if null oneOf
             then [text "_EXPECTNOTHING_"]
             else oneOf

  -- For a predicate with a descriptive label, the label is enough.
  ExpectPredicate (Label lTxt Descriptive) _
    -> [text $ "_PREDICATE_" <> lTxt]

  -- For an enhancing label, we still want to see the rest of the definition.
  ExpectPredicate (Label lTxt Enhancing) mE
    -> map ((text "_PREDICATE_" <> text lTxt) <>) $ maybe [] flattenExpectedDoc mE

  ExpectAnything
    -> [text "ANYTHING"]

  ExpectN i e
    -> [text $ "_EXACTLY_" <> (Text.pack . show $ i) <> "_"
       ,mconcat . flattenExpectedDoc $ e
       ]

  -- A descriptive label is sufficient.
  ExpectLabel (Label lTxt Descriptive) e
    -> [text lTxt]

  -- An enhancing label requires the rest of the definition.
  ExpectLabel (Label lTxt Enhancing) e
    -> [text $ lTxt <> " " <> (render . mconcat . flattenExpectedDoc $ e)
       ]

  ExpectThen e0 e1
    -> [ text . render . mconcat . flattenExpectedDoc $ e0
       , text "_THEN_"
       , text . render . mconcat . flattenExpectedDoc $ e1
       ]

-- Turn an 'Expected' into a list of each expected alternative
flattenExpected :: Expected -> [Text]
flattenExpected = map render . flattenExpectedDoc

instance Document Expected where
  document = showExpectedDoc

-- Turn an 'Expected' into a bulleted list of each unique expected alternative.
showExpected :: Expected -> Text
showExpected = Text.intercalate "\n - "
             . List.nub
             . flattenExpected

instance Document Pos where
  document (Pos t l c) = mconcat
    [text "Line:     ", int l,lineBreak
    ,text "Character:", int c,lineBreak
    ,text "Total:    ", int t,lineBreak
    ]

instance Document Cursor where
  document (Cursor prev next pos) =
    let (before,pointer,after) = point (Cursor prev next pos)
     in mconcat [ rawText before
                , lineBreak
                , text pointer
                , lineBreak
                , document pos
                , lineBreak

                , rawText after
                ]

instance Document a
      => Document (ParseResult a) where
  document p = case p of
    ParseSuccess a leftovers
      -> text "Parsed: " <> document a <> text "with leftovers" <> document leftovers

    ParseFailure failures cur0
      -> mconcat $
           [ text "Parse failure at:"
           , lineBreak

           , indent1 $ document cur0
           , lineBreak
           ]
           ++
           if null failures
             then mempty
             else [ text "The failures backtracked from were:"
                  , lineBreak
                  , indent1 . mconcat
                            . map (\(expected,cursor) -> mconcat [document cursor
                                                                 , text "Expected:"
                                                                 , document expected
                                                                 , lineBreak
                                                                 ]
                                  )
                            . Map.toList
                            . collectFailures
                            $ failures
                  ]


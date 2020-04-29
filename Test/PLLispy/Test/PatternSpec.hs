{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , RankNTypes
  #-}
module PLLispy.Test.PatternSpec where

import PL
import PL.Commented
import PL.Var
import PL.Type
import PL.Expr
import PL.TyVar
import PL.Error

import PL.Test.Parsing.Pattern
import PL.Test.Pattern
import PL.Test.Pattern
import PL.Test.PatternTestCase
import PL.Test.Pattern.Bind
import PL.Test.Pattern.Sum
import PL.Test.Pattern.Product
import PL.Test.Pattern.Union
import PL.Test.Pattern.Binding
import PL.Test.Source
import PL.Pattern

import PLLispy
import PLLispy.Test.Sources.Pattern
import PLLispy.Expr
import PLLispy.Type
import PLLispy.Pattern
import PLLispy.Level

import PLParser
import PLPrinter
import PLPrinter.Doc
import PLGrammar

import Data.Text
import Data.Maybe

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map as Map

import Control.Monad

import Test.Hspec

-- Test Case analysis patterns parse, reduce and type check from example
-- sources
spec
  :: Spec
spec = do
  testKeyPrograms
  testParsePrint

testKeyPrograms :: Spec
testKeyPrograms =
  describe "There must be some input that parses all key programs" $
    parsesToPatternsSpec patternTestCases lispyParser ppPattern (ppError ppPattern ppType)
  where
    patternTestCases :: Map.Map Text.Text PatternTestCase
    patternTestCases = mkPatternTestCases sources

    typeGrammar :: Grammar CommentedType
    typeGrammar = top $ typ tyVar

    ppType :: TypeFor DefaultPhase -> Doc
    ppType = fromMaybe mempty . pprint (toPrinter typeGrammar) . addTypeComments

    patternGrammar :: Grammar CommentedPattern
    patternGrammar = top $ pattern var tyVar

    ppPattern :: PatternFor DefaultPhase -> Doc
    ppPattern = fromMaybe mempty . pprint (toPrinter patternGrammar) . addPatternComments

    lispyParser :: Text.Text -> Either (Error Type Pattern) (PatternFor CommentedPhase, Source)
    lispyParser input = let p = toParser patternGrammar
                         in case runParser p input of
                              ParseSuccess a cursor
                                -> Right (a, remainder cursor)

                              failure
                                -> Left . EMsg . ppParseResult (fromMaybe mempty . pprint (toPrinter patternGrammar)) $ failure

    ppParseResult
      :: (a -> Doc)
      -> PLParser.ParseResult a
      -> Doc
    ppParseResult ppA p = case p of
        PLParser.ParseSuccess a leftovers
          -> text "Parsed: " <> ppA a <> text "with leftovers" <> document leftovers

        PLParser.ParseFailure failures cur0
          -> mconcat $
               [ text "Parse failure at:"
               , lineBreak

               , indent1 $ document cur0
               , lineBreak
               ]
               ++
               if List.null failures
                 then mempty
                 else [ text "The failures backtracked from were:"
                      , lineBreak
                      , indent1 . mconcat
                                . fmap (\(cursor,expected) -> mconcat [ document cursor
                                                                      , document expected
                                                                      , lineBreak
                                                                      , lineBreak
                                                                      ]
                                      )
                                . Map.toList
                                . PLParser.collectFailures
                                $ failures
                      ]


testParsePrint :: Spec
testParsePrint = describe "Lispy specific parse-print behaves" $ do
  describe "Match args" $ do
    describe "Bind" $ do
      testcase $ TestCase
        { _testCase             = "Bind anything"
        , _input                = [ "?"
                                  , "(?)"
                                  ]
        , _grammar              = patternGrammar
        , _shouldParse          = Just Bind
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "?"
        }

    describe "Binding" $ do
      testcase $ TestCase
        { _testCase             = "Match a binding"
        , _input                = [ "0"
                                  , "(0)"
                                  ]
        , _grammar              = patternGrammar
        , _shouldParse          = Just $ BindingPattern $ VZ
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "0"
        }

    describe "Product" $ do
      testcase $ TestCase
        { _testCase             = "Match empty product"
        , _input                = ["*"
                                  ,"(*)"
                                  ]
        , _grammar              = patternGrammar
        , _shouldParse          = Just EmptyProductPattern
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*"
        }

      testcase $ TestCase
        { _testCase             = "Match singleton product"
        , _input                = ["* (?)"
                                  ,"(* (?))"
                                  ]
        , _grammar              = patternGrammar
        , _shouldParse          = Just $ ProductPattern [Bind]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*?"
        }

      testcase $ TestCase
        { _testCase             = "Match two product"
        , _input                = ["* (?) (?)"
                                  ,"(* (?) (?))"
                                  ]
        , _grammar              = patternGrammar
        , _shouldParse          = Just $ ProductPattern [Bind,Bind]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*? ?"
        }

    describe "Sum" $ do
      testcase $ TestCase
        { _testCase             = "Bind a singleton sum"
        , _input                = [ "+0 (?)"
                                  , "(+0 (?))"
                                  ]
        , _grammar              = patternGrammar
        , _shouldParse          = Just $ SumPattern 0 Bind
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+0 ?"
        }

    describe "Union" $ do
      testcase $ TestCase
        { _testCase             = "Bind a singleton union"
        , _input                = [ "U (Foo) (?)"
                                  ]
        , _grammar              = patternGrammar
        , _shouldParse          = Just $ UnionPattern (Named "Foo") $ Bind
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪Foo ?"
        }
  where
    patternGrammar :: Grammar (PatternFor CommentedPhase)
    patternGrammar = top $ pattern var tyVar

-- Test that some source code behaves correctly with parsing and printing
testcase :: (Show a, Eq a) => TestCase a -> Spec
testcase (TestCase name inputs grammar shouldParse shouldParseLeftovers shouldPrint) = describe (Text.unpack name) $ do
  let parser  = toParser  grammar
      printer = toPrinter grammar

  describe "All inputs should parse to the expected value" $ do
    forM_ inputs $ \input -> testParse input parser (shouldParseLeftovers, shouldParse)

  describe "Value should print as expected" $ testPrint shouldParse printer shouldPrint

  describe "Printed text should parse back to ensure roundtrip properties" $ case shouldPrint of
    Nothing
      -> pure ()
    Just p
      -> testParse p parser ("", shouldParse)

testParse :: (Show a, Eq a) => Text -> Parser a -> (Text,Maybe a) -> Spec
testParse input parser (shouldParseLeftovers, shouldParse) =
    let parseResult = runParser parser input
     in case parseResult of
          ParseSuccess a cur
            -> do it "has correct leftovers" $ remainder cur `shouldBe` shouldParseLeftovers
                  it "has correct result" $ Just a `shouldBe` shouldParse
          ParseFailure failures cur
            -> do it "has correct leftovers" $ remainder cur `shouldBe` shouldParseLeftovers
                  it "has correct result" $ case shouldParse of
                     Nothing
                       -> pure () -- TODO: We might want to check the specific failure

                     Just p
                       -> expectationFailure $ Text.unpack $ render $ mconcat $
                             [ text "Parse failure at:"
                             , lineBreak

                             , indent1 $ document cur
                             , lineBreak
                             ]
                             ++
                             if List.null failures
                               then mempty
                               else [ text "The failures backtracked from were:"
                                    , lineBreak
                                    , indent1 . mconcat
                                              . fmap (\(cursor,expected) -> mconcat [ document cursor
                                                                                    , document expected
                                                                                    , lineBreak
                                                                                    , lineBreak
                                                                                    ]
                                                    )
                                              . Map.toList
                                              . collectFailures
                                              $ failures
                                    ]

testPrint :: Maybe a -> Printer a -> Maybe Text -> Spec
testPrint input printer shouldPrint = case input of
    -- If there is no value theres nothing to print?
    Nothing
      -> pure ()
    Just v
      -> it "print result" $ (render <$> pprint printer v) `shouldBe` shouldPrint

data TestCase a = TestCase
  { _testCase             :: Text
  , _input                :: [Text]
  , _grammar              :: Grammar a
  , _shouldParse          :: Maybe a
  , _shouldParseLeftovers :: Text
  , _shouldPrint          :: Maybe Text
  }


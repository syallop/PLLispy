{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , RankNTypes
  #-}
module PLLispy.Test.PatternSpec where

import PL.Commented
import PL.Var
import PL.Type
import PL.FixPhase
import PL.Error

import PL.Test.Parsing.Pattern
import PL.Test.Pattern
import PL.Test.PatternTestCase
import PL.Test.Source
import PL.Pattern

import PLLispy
import PLLispy.Test.Sources.Pattern
import PLLispy.Test.ExprSpec

import PLParser
import PLParser.Diagnostics
import PLParser.State
import PLPrinter
import PLGrammar

import Data.Text
import Data.Maybe

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
    parsesToPatternsSpec patternTestCases lispyParser ppCommentedPattern ppDefaultError
  where
    patternTestCases :: Map.Map Text.Text PatternTestCase
    patternTestCases = mkPatternTestCases sources

    lispyParser :: Text.Text -> Either Error (PatternFor CommentedPhase, Source)
    lispyParser input = let p = toParser lispyPattern
                         in case runParser p input of
                              Passing st a
                                -> Right (a, remainder . cursor $ st)

                              failure
                                -> Left . EMsg . ppParseResult (fromMaybe mempty . pprint (toPrinter lispyPattern)) $ failure

    ppParseResult
      :: (a -> Doc)
      -> PLParser.Parsing a
      -> Doc
    ppParseResult ppA p = case p of
        PLParser.Passing st a
          -> text "Parsed: " <> ppA a <> text "with leftovers" <> (document . remainder . cursor $ st)

        PLParser.Failing st expected
          -> document . failureSummary st $ expected
        _
          -> text "Parse didnt pass or fail and is either pending or halted waiting for more input."


testParsePrint :: Spec
testParsePrint = describe "Lispy specific parse-print behaves" $ do
  describe "Match args" $ do
    describe "Bind" $ do
      testcase $ TestCase
        { _testCase             = "Bind anything"
        , _input                = [ "?"
                                  , "(?)"
                                  ]
        , _grammar              = commentedPatternGrammar
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
        , _grammar              = commentedPatternGrammar
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
        , _grammar              = commentedPatternGrammar
        , _shouldParse          = Just EmptyProductPattern
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*"
        }

      testcase $ TestCase
        { _testCase             = "Match singleton product"
        , _input                = ["* (?)"
                                  ,"(* (?))"
                                  ]
        , _grammar              = commentedPatternGrammar
        , _shouldParse          = Just $ ProductPattern [Bind]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*?"
        }

      testcase $ TestCase
        { _testCase             = "Match two product"
        , _input                = ["* (?) (?)"
                                  ,"(* (?) (?))"
                                  ]
        , _grammar              = commentedPatternGrammar
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
        , _grammar              = commentedPatternGrammar
        , _shouldParse          = Just $ SumPattern 0 Bind
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+0 ?"
        }

    describe "Union" $ do
      testcase $ TestCase
        { _testCase             = "Bind a singleton union"
        , _input                = [ "U (Foo) (?)"
                                  ]
        , _grammar              = commentedPatternGrammar
        , _shouldParse          = Just $ UnionPattern (Named "Foo") $ Bind
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪Foo ?"
        }

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
testParse input parser (shouldParseLeftovers, shouldParse) = case runParser parser input of
  Passing st a
    -> do it "has correct leftovers" $ (remainder . cursor $ st) `shouldBe` shouldParseLeftovers
          it "has correct result" $ Just a `shouldBe` shouldParse
  Failing st expected
    -> do it "has correct leftovers" $ (remainder . cursor $ st) `shouldBe` shouldParseLeftovers
          it "has correct result" $ case shouldParse of
             Nothing
               -> pure () -- TODO: We might want to check the specific failure

             Just _p
               -> expectationFailure . Text.unpack . render . document . failureSummary st $ expected
  _
    -> it "Has a passing or failing result" $ expectationFailure . Text.unpack . render . text $ "Parse didnt pass or fail and is either pending or halted waiting for more input."

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


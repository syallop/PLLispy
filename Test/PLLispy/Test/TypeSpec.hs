{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , FlexibleContexts
  , RankNTypes
  , TypeFamilies
  #-}
module PLLispy.Test.TypeSpec where

-- Core PL
import PL.Commented
import PL.Error
import PL.FixPhase
import PL.Test.Parsing.Type
import PL.Test.Source
import PL.Test.Type
import PL.Test.TypeTestCase
import PL.Type

-- PL Lispy
import PLLispy
import PLLispy.Test.ExprSpec
import PLLispy.Test.Sources.Type

-- Other PL
import PLGrammar
import PLParser
import PLParser.Diagnostics
import PLParser.State
import PLPrinter

-- Other
import Data.Maybe
import Data.Text
import qualified Data.Map as Map
import qualified Data.Text as Text

import Control.Monad

import Test.Hspec

spec
  :: Spec
spec = do
  testKeyPrograms
  testParsePrint

testKeyPrograms :: Spec
testKeyPrograms =
  describe "There must be some input that parses all key programs" $
    parsesToTypesSpec typeTestCases lispyParser ppCommentedType ppDefaultError
  where
    typeTestCases :: Map.Map Text.Text TypeTestCase
    typeTestCases = mkTypeTestCases sources

    lispyParser :: Text.Text -> Either Error (TypeFor CommentedPhase, Source)
    lispyParser input = let p = toParser lispyType
                         in case runParser p input of
                              Passing st a
                                -> Right (a, remainder . cursor $ st)

                              failure
                                -> Left . EMsg . ppParseResult (fromMaybe mempty . pprint (toPrinter lispyType)) $ failure

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
  describe "Types" $ do
    it "Any tests" pending

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


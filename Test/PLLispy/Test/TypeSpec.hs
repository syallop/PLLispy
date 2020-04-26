{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , RankNTypes
  #-}
module PLLispy.Test.TypeSpec where

import PL
import PL.Var
import PL.Type
import PL.Expr
import PL.TyVar
import PL.Error
import PL.Commented

import PL.Test.Parsing.Type
import PL.Test.Type
import PL.Test.TypeTestCase
import PL.Test.Source

import PLLispy
import PLLispy.Test.Sources.Type
import PLLispy.Expr
import PLLispy.Type
import PLLispy.Type
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
    parsesToTypesSpec typeTestCases lispyParser ppType (ppError ppType)
  where
    typeTestCases :: Map.Map Text.Text TypeTestCase
    typeTestCases = mkTypeTestCases sources

    ppType :: forall phase. TypeFor phase -> Doc
    ppType = undefined
    --ppType = fromMaybe mempty . pprint (toPrinter typeGrammar)

    typeGrammar :: Grammar CommentedType
    typeGrammar = top $ typ tyVar

    lispyParser :: Text.Text -> Either (Error CommentedPhase) (TypeFor CommentedPhase, Source)
    lispyParser input = let p = toParser typeGrammar
                         in case runParser p input of
                              ParseFailure _failures cursor
                                -- TODO: Format failure into error message
                                -> Left $ EMsg (text "failed to parse expression")
                              ParseSuccess a cursor
                                -> Right (a,remainder cursor)


testParsePrint :: Spec
testParsePrint = describe "Lispy specific parse-print behaves" $ do
  describe "Types" $ do
    it "Any tests" pending
  where
    typeGrammar :: Grammar CommentedType
    typeGrammar = top $ typ tyVar

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


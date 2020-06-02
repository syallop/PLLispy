{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , FlexibleContexts
  , RankNTypes
  , TypeFamilies
  #-}
module PLLispy.Test.TypeSpec where

import PL
import PL.Var
import PL.Type
import PL.Expr
import PL.TyVar
import PL.Error
import PL.Commented
import PL.TypeCtx
import PL.FixPhase
import PL.Name

import PL.Test.Parsing.Type
import PL.Test.Type
import PL.Test.TypeTestCase
import PL.Test.Source
import PL.Pattern
import PL.HashStore

import PLLispy
import PLLispy.Test.Sources.Type
import PLLispy.Test.ExprSpec
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
    parsesToTypesSpec typeTestCases lispyParser (ppTestType . addTypeComments) ppTestError
  where
    typeTestCases :: Map.Map Text.Text TypeTestCase
    typeTestCases = mkTypeTestCases sources

    lispyParser :: Text.Text -> Either (Error Expr Type Pattern TypeCtx) (TypeFor CommentedPhase, Source)
    lispyParser input = let p = toParser lispyType
                         in case runParser p input of
                              ParseSuccess a cursor
                                -> Right (a, remainder cursor)

                              failure
                                -> Left . EMsg . ppParseResult (fromMaybe mempty . pprint (toPrinter lispyType)) $ failure

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


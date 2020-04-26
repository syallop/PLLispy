{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , RankNTypes
  #-}
module PLLispy.Test.MatchArgSpec where

import PL
import PL.Commented
import PL.Var
import PL.Type
import PL.Expr
import PL.TyVar
import PL.Error

import PL.Test.Parsing.MatchArg
import PL.Test.MatchArg
import PL.Test.MatchArg
import PL.Test.MatchArgTestCase
import PL.Test.MatchArg.Bind
import PL.Test.MatchArg.Sum
import PL.Test.MatchArg.Product
import PL.Test.MatchArg.Union
import PL.Test.MatchArg.Binding
import PL.Test.Source

import PLLispy
import PLLispy.Test.Sources.MatchArg
import PLLispy.Expr
import PLLispy.Type
import PLLispy.MatchArg
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

-- Test Case analysis matchargs parse, reduce and type check from example
-- sources
spec
  :: Spec
spec = do
  testKeyPrograms
  testParsePrint

testKeyPrograms :: Spec
testKeyPrograms =
  describe "There must be some input that parses all key programs" $
    parsesToMatchArgsSpec matchArgTestCases lispyParser ppMatchArg (ppError ppType)
  where
    matchArgTestCases :: Map.Map Text.Text MatchArgTestCase
    matchArgTestCases = mkMatchArgTestCases sources

    typeGrammar :: Grammar CommentedType
    typeGrammar = top $ typ tyVar

    ppType :: TypeFor DefaultPhase -> Doc
    ppType = fromMaybe mempty . pprint (toPrinter typeGrammar) . addTypeComments

    matchArgGrammar :: Grammar CommentedMatchArg
    matchArgGrammar = top $ matchArg var tyVar

    ppMatchArg :: MatchArgFor DefaultPhase -> Doc
    ppMatchArg = fromMaybe mempty . pprint (toPrinter matchArgGrammar) . addMatchArgComments

    lispyParser :: Text.Text -> Either (Error DefaultPhase) (MatchArgFor CommentedPhase, Source)
    lispyParser input = let p = toParser matchArgGrammar
                         in case runParser p input of
                              ParseSuccess a cursor
                                -> Right (a, remainder cursor)

                              failure
                                -> Left . EMsg . ppParseResult (fromMaybe mempty . pprint (toPrinter matchArgGrammar)) $ failure

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
        , _grammar              = matchArgGrammar
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
        , _grammar              = matchArgGrammar
        , _shouldParse          = Just $ MatchBinding $ VZ
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "0"
        }

    describe "Product" $ do
      testcase $ TestCase
        { _testCase             = "Match empty product"
        , _input                = ["*"
                                  ,"(*)"
                                  ]
        , _grammar              = matchArgGrammar
        , _shouldParse          = Just $ MatchProduct []
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*"
        }

      testcase $ TestCase
        { _testCase             = "Match singleton product"
        , _input                = ["* (?)"
                                  ,"(* (?))"
                                  ]
        , _grammar              = matchArgGrammar
        , _shouldParse          = Just $ MatchProduct [Bind]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*?"
        }

      testcase $ TestCase
        { _testCase             = "Match two product"
        , _input                = ["* (?) (?)"
                                  ,"(* (?) (?))"
                                  ]
        , _grammar              = matchArgGrammar
        , _shouldParse          = Just $ MatchProduct [Bind,Bind]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*? ?"
        }

    describe "Sum" $ do
      testcase $ TestCase
        { _testCase             = "Bind a singleton sum"
        , _input                = [ "+0 (?)"
                                  , "(+0 (?))"
                                  ]
        , _grammar              = matchArgGrammar
        , _shouldParse          = Just $ MatchSum 0 Bind
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+0 ?"
        }

    describe "Union" $ do
      testcase $ TestCase
        { _testCase             = "Bind a singleton union"
        , _input                = [ "U (Foo) (?)"
                                  ]
        , _grammar              = matchArgGrammar
        , _shouldParse          = Just $ MatchUnion (Named "Foo") $ Bind
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪Foo ?"
        }
  where
    matchArgGrammar :: Grammar (MatchArgFor CommentedPhase)
    matchArgGrammar = top $ matchArg var tyVar

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


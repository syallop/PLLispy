{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module PLLispy.Test.ExprSpec
  ( -- Test PL expressions can be parsed from some lispy source and that the parsing-printing has roundtrip properties
    spec

   -- Misc utilities used for testing that other *Spec modules would like to
   -- resuse.
  , TestExpr
  , TestType
  , TestPattern
  , TestTypeCtx
  , ppTestExpr
  , ppTestType
  , ppTestPattern
  , ppVar
  , ppTyVar
  , ppTestError
  )
  where

import PL
import PL.Case
import PL.Pattern
import PL.Commented
import PL.Error
import PL.Expr
import PL.Kind
import PL.TyVar
import PL.Type
import PL.Name
import PL.Hash
import PL.Var
import PL.TypeCtx

import PL.Test.ExprTestCase
import PL.Test.Parsing.Expr
import PL.Test.Expr
import PL.Test.Expr.BigLam
import PL.Test.Expr.Boolean
import PL.Test.Expr.Function
import PL.Test.Expr.Lam
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union
import PL.Test.Source

import PLLispy
import PLLispy.Expr
import PLLispy.Type
import PLLispy.Test.Sources.Expr
import PLLispy.Level

import PLGrammar
import PLPrinter
import PLPrinter.Doc
import PLParser

import Control.Monad
import Data.Text
import qualified Data.Text as Text
import Data.Monoid hiding (Product, Sum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.List as List

import Test.Hspec

type TestExpr    = ExprFor    CommentedPhase
type TestType    = TypeFor    CommentedPhase
type TestPattern = PatternFor CommentedPhase
type TestTypeCtx = TypeCtxFor CommentedPhase

ppTestExpr :: TestExpr -> Doc
ppTestExpr = fromMaybe mempty . pprint (toPrinter lispyExpr)

ppTestType :: TestType -> Doc
ppTestType = fromMaybe mempty . pprint (toPrinter lispyType)

ppTestPattern :: TestPattern -> Doc
ppTestPattern = fromMaybe mempty . pprint (toPrinter lispyPattern)

ppVar :: Var -> Doc
ppVar = fromMaybe mempty . pprint (toPrinter var)

ppTyVar :: TyVar -> Doc
ppTyVar = fromMaybe mempty . pprint (toPrinter tyVar)

ppTestError :: Error Expr Type Pattern TypeCtx -> Doc
ppTestError = ppError (ppTestPattern . addPatternComments)
                      (ppTestType . addTypeComments)
                      (ppTestExpr . addComments)
                      (ppTypeCtx document (ppTypeInfo (ppTestType . addTypeComments)))
                      ppVar
                      ppTyVar


-- Test expressions parse, reduce and type check from example sources
spec
  :: Spec
spec = do
  testKeyPrograms
  testParsePrint

testKeyPrograms :: Spec
testKeyPrograms =
  describe "There must be some input that parses all key programs" $
    parsesToSpec exprTestCases lispyParser (ppTestExpr . addComments) ppTestError
  where
    exprTestCases :: Map.Map Text.Text ExprTestCase
    exprTestCases = mkTestCases sources

    lispyParser :: Text.Text -> Either (Error Expr Type Pattern TypeCtx) (ExprFor CommentedPhase, Source)
    lispyParser input = let p = toParser lispyExpr
                         in case runParser p input of
                              ParseSuccess a cursor
                                -> Right (a,remainder cursor)

                              failure
                                -> Left . EMsg . ppParseResult (fromMaybe mempty . pprint (toPrinter lispyExpr)) $ failure
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
  describe "Expressions" $ do
    describe "Lambdas" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["\\Bool (0)" -- 'standard' form
                                  ,"\\Bool 0"   -- Dropping all uneccesary parens
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Lam (Named $ "Bool") (Binding $ VZ)
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "λBool 0"
        }

      testcase $ TestCase
        { _testCase             = "Complex type argument"
        , _input                = ["\\(+Bool Nat) (0)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Lam (SumT $ NE.fromList [Named $ "Bool", Named $ "Nat"]) (Binding $ VZ)
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "λ(+Bool Nat) 0"
        }

    describe "Application" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["@ (0) (1)"
                                  ,"@ 0 1"
                                  ,"@ (0) 1"
                                  ,"@ 0 (1)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ App (Binding $ VZ) (Binding $ VS $ VZ)
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "@0 1"
        }

    describe "Binding" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["0"
                                  ,"(0)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Binding $ VZ
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "0"
        }

    describe "ContentBinding" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["#SHA512/5x9U6wV2TtRKERLUJPdbnx8tyHPHAsban2gPAHsHHYGhVys1mDZT24WaEnVujtDepQv3nP7ff4gcqBR5hPmLxDNFC4RFHwjuxodJw56wAVQcui1dgkiky5hsYfBg7WA5o2ZFBwhtkysYVLJaDiTyHLnFeZECDtjDYpzo6cz5LxRfos6"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just . ContentBinding . mkContentName . fromJust . readBase58 $ "SHA512/5x9U6wV2TtRKERLUJPdbnx8tyHPHAsban2gPAHsHHYGhVys1mDZT24WaEnVujtDepQv3nP7ff4gcqBR5hPmLxDNFC4RFHwjuxodJw56wAVQcui1dgkiky5hsYfBg7WA5o2ZFBwhtkysYVLJaDiTyHLnFeZECDtjDYpzo6cz5LxRfos6"
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "#SHA512/5x9U6wV2TtRKERLUJPdbnx8tyHPHAsban2gPAHsHHYGhVys1mDZT24WaEnVujtDepQv3nP7ff4gcqBR5hPmLxDNFC4RFHwjuxodJw56wAVQcui1dgkiky5hsYfBg7WA5o2ZFBwhtkysYVLJaDiTyHLnFeZECDtjDYpzo6cz5LxRfos6"
        }

    describe "Sum" $ do
      testcase $ TestCase
        { _testCase             = "Sum of empty product"
        , _input                = ["+0 (*) (*)"
                                  ,"+0 (*) *"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Sum EmptyProduct 0 $ NE.fromList $ [EmptyProductT]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+0(*) (*)"
        }

      testcase $ TestCase
        { _testCase             = "Sum of product of product"
        , _input                = ["+0 (* (*)) (* (*))"
                                  ,"+0 (* *) (* *)"
                                  ,"+0 (* *) * *"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Sum (Product [EmptyProduct]) 0 $ NE.fromList $ [ProductT [EmptyProductT]]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+0(*(*)) (*(*))"
        }

    describe "Product values" $ do
      testcase $ TestCase
        { _testCase             = "Naked, empty product"
        , _input                = ["*"
                                  ,"(*)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ EmptyProduct
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*"
        }

      testcase $ TestCase
        { _testCase             = "Singleton product of empty product"
        , _input                = ["* (*)"
                                  ,"(* (*))"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Product [EmptyProduct]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*(*)"
        }

      testcase $ TestCase
        { _testCase             = "Product of two empty products"
        , _input                = [ "* (*) (*)"
                                  , "(* (*) (*))"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Product [EmptyProduct,EmptyProduct]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*(*) (*)"
        }

    describe "Union values" $ do
      testcase $ TestCase
        { _testCase             = "Singleton union"
        , _input                = ["U (*) (*) (*)"
                                  ,"(U (*) (*) (*))"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ Union EmptyProduct EmptyProductT (Set.fromList [EmptyProductT])
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪(*) (*) (*)"
        }

    -- Note the 'Pattern' patterns used in case branches are tested in
    -- the PatternSpec module. These tests only cover the outer structure of
    -- the case statement itself.
    describe "Case" $ do
      testcase $ TestCase
        { _testCase             = "Default branch only"
        , _input                = ["CASE (0) (*)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ CaseAnalysis $ Case (Binding $ VZ) $ DefaultOnly $ EmptyProduct
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (*)"
        }

      testcase $ TestCase
        { _testCase             = "Single branch only"
        , _input                = ["CASE (0) (| (?) (*))"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ CaseAnalysis $ Case (Binding $ VZ) $ CaseBranches (NE.fromList [CaseBranch Bind $ EmptyProduct]) Nothing
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (|? (*))"
        }

      testcase $ TestCase
        { _testCase             = "Single branch and default"
        , _input                = ["CASE (0) (|(?) (*)) (*)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ CaseAnalysis $ Case (Binding $ VZ) $ CaseBranches (NE.fromList [CaseBranch Bind $ EmptyProduct]) (Just $ EmptyProduct)
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (|? (*)) (*)"
        }

      testcase $ TestCase
        { _testCase             = "Multiple branches no default"
        , _input                = ["CASE (0) (| (?) (*)) (|(?) (*))"
                                  ,"(CASE (0)\n\
                                   \       (| (?) (*))\n\
                                   \       (| (?) (*))\n\
                                   \)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ CaseAnalysis $ Case (Binding $ VZ) $ CaseBranches (let b = CaseBranch Bind $ EmptyProduct in NE.fromList [b,b]) Nothing
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (|? (*)) (|? (*))"
        }

      testcase $ TestCase
        { _testCase             = "Multiple branches and default"
        , _input                = ["CASE (0) (|(?) (*)) (|(?) (*)) (*)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ CaseAnalysis $ Case (Binding $ VZ) $ CaseBranches (let b = CaseBranch Bind $ EmptyProduct in NE.fromList [b,b]) (Just $ EmptyProduct)
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (|? (*)) (|? (*)) (*)"
        }

      testcase $ TestCase
        { _testCase             = "No branches or default"
        , _input                = ["CASE (0)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Nothing
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Nothing
        }


  describe "Types" $ do
    describe "Named" $ do
      testcase $ TestCase
        { _testCase             = "Valid name"
        , _input                = ["Name"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ Named "Name"
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "Name"
        }

    describe "Arrow" $ do
      testcase $ TestCase
        { _testCase             = "Simplest function type"
        , _input                = ["→ (Bool) (Nat)"
                                  ,"→ Bool Nat"
                                  ,"-> Bool Nat"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ Arrow (Named "Bool") (Named "Nat")
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "→Bool Nat"
        }

    describe "Sum" $ do
      testcase $ TestCase
        { _testCase             = "Singleton sum"
        , _input                = ["+ (Bool)"
                                  ,"+ Bool"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ SumT $ NE.fromList [Named "Bool"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+Bool"
        }

      testcase $ TestCase
        { _testCase             = "Sum of two"
        , _input                = ["+ (Bool) (Nat)"
                                  ,"+ Bool Nat"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ SumT $ NE.fromList [Named "Bool", Named "Nat"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+Bool Nat"
        }

    describe "Product" $ do
      testcase $ TestCase
        { _testCase             = "Empty product"
        , _input                = ["*"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ EmptyProductT
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*"
        }

      testcase $ TestCase
        { _testCase             = "Singleton product"
        , _input                = ["* (Bool)"
                                  ,"* Bool"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ ProductT [Named "Bool"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*Bool"
        }

      testcase $ TestCase
        { _testCase             = "Two product"
        , _input                = ["* (Bool) (Nat)"
                                  ,"* Bool Nat"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ ProductT [Named "Bool", Named "Nat"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*Bool Nat"
        }

    describe "Union" $ do
      -- TODO: There is currently no way to construct an empty union, similar to
      -- the empty set. Should union be modified to reflect this?
      testcase $ TestCase
        { _testCase             = "Empty union"
        , _input                = ["U"
                                  ,"∪"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ UnionT $ Set.fromList []
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪"
        }

      testcase $ TestCase
        { _testCase             = "Singleton union"
        , _input                = ["U (Bool)"
                                  ,"U Bool"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ UnionT $ Set.fromList [Named "Bool"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪Bool"
        }

      testcase $ TestCase
        { _testCase             = "Two union"
        , _input                = ["U (Bool) (Nat)"
                                  ,"U (Nat) (Bool)"
                                  ,"U Bool Nat"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ UnionT $ Set.fromList [Named "Bool", Named "Nat"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪Bool Nat"
        }

    describe "Big Arrow" $ do
      testcase $ TestCase
        { _testCase             = "Simplest big arrow"
        , _input                = ["/-> (KIND) (Bool)"
                                  ,"/-> KIND Bool"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ BigArrow Kind (Named "Bool")
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "/->KIND Bool"
        }

    describe "Type Lam" $ do
      testcase $ TestCase
        { _testCase             = "Simplest type lambda"
        , _input                = ["/\\ (KIND) (Bool)"
                                  ,"/\\ KIND Bool"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ TypeLam Kind (Named "Bool")
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "Λ(KIND) Bool"
        }

    describe "Type App" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["/@ (Bool) (Nat)"
                                  ,"/@ Bool Nat"
                                  ,"/@ (Bool) Nat"
                                  ,"/@ Bool (Nat)"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ TypeApp (Named "Bool") (Named "Nat")
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "/@Bool Nat"
        }

    describe "Type binding" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["?0"
                                  ,"(?0)"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ TypeBinding $ TyVar $ VZ
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "?0"
        }

  where
    exprGrammar :: Grammar (ExprFor CommentedPhase)
    exprGrammar = lispyExpr

    typeGrammar :: Grammar (TypeFor CommentedPhase)
    typeGrammar = lispyType

data TestCase a = TestCase
  { _testCase             :: Text
  , _input                :: [Text]
  , _grammar              :: Grammar a
  , _shouldParse          :: Maybe a
  , _shouldParseLeftovers :: Text
  , _shouldPrint          :: Maybe Text
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


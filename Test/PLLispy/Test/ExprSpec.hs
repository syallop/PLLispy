{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module PLLispy.Test.ExprSpec where

import PL
import PL.Expr
import PL.FixExpr
import PL.Var
import PL.TyVar
import PL.Case
import PL.Type
import PL.Kind
import PL.FixType
import PL.Test.Expr
import PL.Test.Expr.BigLam
import PL.Test.Expr.Boolean
import PL.Test.Expr.Function
import PL.Test.Expr.Lam
import PL.Test.Expr.Natural
import PL.Test.Expr.Product
import PL.Test.Expr.Sum
import PL.Test.Expr.Union

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

-- Test expressions parse, reduce and type check from example sources
spec
  :: Spec
spec = do
  testKeyPrograms
  testParsePrint

testKeyPrograms :: Spec
testKeyPrograms = describe "Test whether we can parse key programs (which must then type check and reduce correctly)" $ parserSpec sources lispyParser ppExpr ppType
  where
    typeGrammar :: Grammar (Type TyVar)
    typeGrammar = top $ typ tyVar

    ppType :: Type TyVar -> Doc
    ppType = fromMaybe mempty . pprint (toPrinter typeGrammar)

    exprGrammar :: Grammar (Expr Var (Type TyVar) TyVar)
    exprGrammar = top (expr var typeGrammar tyVar)

    ppExpr :: Expr Var (Type TyVar) TyVar -> Doc
    ppExpr = fromMaybe mempty . pprint (toPrinter exprGrammar)

    lispyParser = toParser exprGrammar

testParsePrint :: Spec
testParsePrint = describe "Lispy specific parse-print behaves" $ do
  describe "Expressions" $ do
    describe "Lambdas" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["\\Foo (0)" -- 'standard' form
                                  ,"\\Foo 0"   -- Dropping all uneccesary parens
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ Lam {_take = FixType $ Named $ "Foo"
                                                       ,_expr = FixExpr $ Binding $ VZ
                                                       }
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "λFoo 0"
        }

      testcase $ TestCase
        { _testCase             = "Complex type argument"
        , _input                = ["\\(+Foo Bar) (0)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ Lam {_take = FixType $ SumT $ NE.fromList [FixType $ Named $ "Foo", FixType $ Named $ "Bar"]
                                                       ,_expr = FixExpr $ Binding $ VZ
                                                       }
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "λ(+Foo Bar) 0"
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
        , _shouldParse          = Just $ FixExpr $ App {_f = FixExpr $ Binding $ VZ
                                                       ,_x = FixExpr $ Binding $ VS $ VZ
                                                       }
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
        , _shouldParse          = Just $ FixExpr $ Binding $ VZ
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "0"
        }

    describe "Sum" $ do
      testcase $ TestCase
        { _testCase             = "Sum of empty product"
        , _input                = ["+0 (*) (*)"
                                  ,"+0 (*) *"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ Sum (FixExpr $ Product []) 0 $ NE.fromList $ [FixType $ ProductT []]
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
        , _shouldParse          = Just $ FixExpr $ Sum (FixExpr $ Product [FixExpr $ Product []]) 0 $ NE.fromList $ [FixType $ ProductT [FixType $ ProductT []]]
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
        , _shouldParse          = Just $ FixExpr $ Product []
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*"
        }

      testcase $ TestCase
        { _testCase             = "Singleton product of empty product"
        , _input                = ["* (*)"
                                  ,"(* (*))"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ Product [FixExpr $ Product []]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*(*)"
        }

      testcase $ TestCase
        { _testCase             = "Product of two empty products"
        , _input                = [ "* (*) (*)"
                                  , "(* (*) (*))"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ Product [FixExpr $ Product [],FixExpr $ Product []]
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
        , _shouldParse          = Just $ FixExpr $ Union (FixExpr $ Product []) (FixType $ ProductT []) (Set.fromList [FixType $ ProductT []])
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪(*) (*) (*)"
        }

    -- Note the 'MatchArg' patterns used in case branches are tested in
    -- the MatchArgSpec module. These tests only cover the outer structure of
    -- the case statement itself.
    describe "Case" $ do
      testcase $ TestCase
        { _testCase             = "Default branch only"
        , _input                = ["CASE (0) (*)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ CaseAnalysis $ Case (FixExpr $ Binding $ VZ) $ DefaultOnly $ FixExpr $ Product []
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (*)"
        }

      testcase $ TestCase
        { _testCase             = "Single branch only"
        , _input                = ["CASE (0) (| (?) (*))"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ CaseAnalysis $ Case (FixExpr $ Binding $ VZ) $ CaseBranches (NE.fromList [CaseBranch Bind $ FixExpr $ Product []]) Nothing
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (|? (*))"
        }

      testcase $ TestCase
        { _testCase             = "Single branch and default"
        , _input                = ["CASE (0) (|(?) (*)) (*)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ CaseAnalysis $ Case (FixExpr $ Binding $ VZ) $ CaseBranches (NE.fromList [CaseBranch Bind $ FixExpr $ Product []]) (Just $ FixExpr $ Product [])
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
        , _shouldParse          = Just $ FixExpr $ CaseAnalysis $ Case (FixExpr $ Binding $ VZ) $ CaseBranches (let b = CaseBranch Bind $ FixExpr $ Product [] in NE.fromList [b,b]) Nothing
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "CASE 0 (|? (*)) (|? (*))"
        }

      testcase $ TestCase
        { _testCase             = "Multiple branches and default"
        , _input                = ["CASE (0) (|(?) (*)) (|(?) (*)) (*)"
                                  ]
        , _grammar              = exprGrammar
        , _shouldParse          = Just $ FixExpr $ CaseAnalysis $ Case (FixExpr $ Binding $ VZ) $ CaseBranches (let b = CaseBranch Bind $ FixExpr $ Product [] in NE.fromList [b,b]) (Just $ FixExpr $ Product [])
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
        , _shouldParse          = Just $ FixType $ Named "Name"
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "Name"
        }

    describe "Arrow" $ do
      testcase $ TestCase
        { _testCase             = "Simplest function type"
        , _input                = ["→ (Foo) (Bar)"
                                  ,"→ Foo Bar"
                                  ,"-> Foo Bar"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ Arrow (FixType $ Named "Foo") (FixType $ Named "Bar")
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "→Foo Bar"
        }

    describe "Sum" $ do
      testcase $ TestCase
        { _testCase             = "Singleton sum"
        , _input                = ["+ (Foo)"
                                  ,"+ Foo"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ SumT $ NE.fromList [FixType $ Named "Foo"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+Foo"
        }

      testcase $ TestCase
        { _testCase             = "Sum of two"
        , _input                = ["+ (Foo) (Bar)"
                                  ,"+ Foo Bar"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ SumT $ NE.fromList [FixType $ Named "Foo", FixType $ Named "Bar"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "+Foo Bar"
        }

    describe "Product" $ do
      testcase $ TestCase
        { _testCase             = "Empty product"
        , _input                = ["*"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ ProductT []
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*"
        }

      testcase $ TestCase
        { _testCase             = "Singleton product"
        , _input                = ["* (Foo)"
                                  ,"* Foo"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ ProductT [FixType $ Named "Foo"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*Foo"
        }

      testcase $ TestCase
        { _testCase             = "Two product"
        , _input                = ["* (Foo) (Bar)"
                                  ,"* Foo Bar"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ ProductT [FixType $ Named "Foo", FixType $ Named "Bar"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "*Foo Bar"
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
        , _shouldParse          = Just $ FixType $ UnionT $ Set.fromList []
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪"
        }

      testcase $ TestCase
        { _testCase             = "Singleton union"
        , _input                = ["U (Foo)"
                                  ,"U Foo"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ UnionT $ Set.fromList [FixType $ Named "Foo"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪Foo"
        }

      testcase $ TestCase
        { _testCase             = "Two union"
        , _input                = ["U (Foo) (Bar)"
                                  ,"U (Bar) (Foo)"
                                  ,"U Foo Bar"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ UnionT $ Set.fromList [FixType $ Named "Foo", FixType $ Named "Bar"]
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "∪Bar Foo"
        }

    describe "Big Arrow" $ do
      testcase $ TestCase
        { _testCase             = "Simplest big arrow"
        , _input                = ["/-> (KIND) (Foo)"
                                  ,"/-> KIND Foo"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ BigArrow Kind (FixType $ Named "Foo")
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "/->KIND Foo"
        }

    describe "Type Lam" $ do
      testcase $ TestCase
        { _testCase             = "Simplest type lambda"
        , _input                = ["/\\ (KIND) (Foo)"
                                  ,"/\\ KIND Foo"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ TypeLam Kind (FixType $ Named "Foo")
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "Λ(KIND) Foo"
        }

    describe "Type App" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["/@ (Foo) (Bar)"
                                  ,"/@ Foo Bar"
                                  ,"/@ (Foo) Bar"
                                  ,"/@ Foo (Bar)"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ TypeApp {_f = FixType $ Named "Foo"
                                                           ,_x = FixType $ Named "Bar"
                                                           }
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "/@Foo Bar"
        }

    describe "Type binding" $ do
      testcase $ TestCase
        { _testCase             = "Simple"
        , _input                = ["?0"
                                  ,"(?0)"
                                  ]
        , _grammar              = typeGrammar
        , _shouldParse          = Just $ FixType $ TypeBinding $ TyVar $ VZ
        , _shouldParseLeftovers = ""
        , _shouldPrint          = Just "?0"
        }

  where
    exprGrammar :: Grammar (Expr Var (Type TyVar) TyVar)
    exprGrammar = top (expr var (sub $ typ tyVar) tyVar)

    typeGrammar :: Grammar (Type TyVar)
    typeGrammar = top $ typ tyVar

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


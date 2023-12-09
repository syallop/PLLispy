{-# LANGUAGE
    ConstraintKinds
  , ImplicitParams
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , FlexibleContexts
  , GADTs
  , TypeOperators
  #-}
{-|
Module      : PLLispy.Expr
Copyright   : (c) Samuel A. Yallop, 2016
Maintainer  : syallop@gmail.com
Stability   : experimental

A Grammar for PL.Expr with a lisp-like syntax, parameterised over grammars for
dependencies such as bindings and abstractions as well as possible constructor
extensions.
-}
module PLLispy.Expr
  (
  -- * Concrete expressions
  --
  -- Grammar for a lispy expression that depends upon grammars for things such
  -- as bindings, abstractions etc.
    expr
  , GrammarDependencies (..)
  , defaultGrammarDependencies

  -- * Expr constructors
  -- Grammars for alternatives in a general 'ExprFor phase' ast that defer to implicit
  -- dependent grammars where necessary.
  , exprI
  , lamExpr
  , appExpr
  , bindingExpr
  , contentBindingExpr
  , caseAnalysisExpr
  , sumExpr
  , productExpr
  , unionExpr
  , bigLamExpr
  , bigAppExpr
  , exprExtensionExpr

  -- * Expr extension Grammars
  --
  -- Grammars for specific extensions to the Expr ast itself.
  , commentedExpr

  -- * Grammar Dependency options
  --
  -- Grammars that are likely to be used to instantiate the dependencies

  -- ** Expr bindings
  , var

  -- ** Expr abstractions
  , typ

  -- ** ContentBindings at Expr and Type level
  , shortHash
  , contentName
  )
  where

-- Lispy
import PLLispy.Case
import PLLispy.Expr.Dep
import PLLispy.Expr.Iso
import PLLispy.Kind
import PLLispy.Level
import PLLispy.Name
import PLLispy.Pattern
import PLLispy.Type

-- Core PL
import PL.Case
import PL.Commented
import PL.Expr hiding (appise,lamise)
import PL.FixPhase
import PL.Pattern
import PL.TyVar
import PL.Type
import PL.Var

-- Other PL
import PLHash.Short
import PLGrammar
import PLLabel
import Reversible
import Reversible.Iso

-- Other
import Data.Text

defaultGrammarDependencies
  :: ( Constraints phase

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

     , NoExt ~ SumPatternExtension phase
     , NoExt ~ ProductPatternExtension phase
     , NoExt ~ UnionPatternExtension phase
     , NoExt ~ BindingPatternExtension phase
     , NoExt ~ BindExtension phase

     , (Commented (ExprFor phase)) ~ ExprExtension phase
     , (Commented (TypeFor phase)) ~ TypeExtension phase
     , (Commented (PatternFor phase)) ~ PatternExtension phase
     )
  => GrammarDependencies phase
defaultGrammarDependencies = GrammarDependencies
  { _bindingFor                = var
  , _contentBindingFor         = shortHash
  , _abstractionFor            = sub $ typ defaultTypeGrammarDependencies
  , _exprTypeBindingFor        = tyVar
  , _exprTypeContentBindingFor = shortHash

  , _lamGrammarExtension            = noExtG
  , _appGrammarExtension            = noExtG
  , _bindingGrammarExtension        = noExtG
  , _contentBindingGrammarExtension = noExtG
  , _caseAnalysisGrammarExtension   = noExtG
  , _sumGrammarExtension            = noExtG
  , _productGrammarExtension        = noExtG
  , _unionGrammarExtension          = noExtG
  , _bigLamGrammarExtension         = noExtG
  , _bigAppGrammarExtension         = noExtG

  , _exprGrammarExtension = commentedExpr defaultGrammarDependencies defaultTypeGrammarDependencies defaultPatternGrammarDependencies
  }

-- | The lambda constructor takes the form:
--
-- LAMBDA ABS EXPR
lamExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
lamExpr = label (enhancingLabel "Lambda") $
  lambda */
  (lamIso \$/ lamExtension
          \*/ (spaceAllowed */ abstraction)
          \*/ (spaceRequired */ sub exprI))

-- | The Big lambda constructor takes the form:
--
-- BIGLAMBDA kind EXPR
bigLamExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
bigLamExpr = label (enhancingLabel "Big lambda") $
  bigLambda */
  (bigLamIso \$/ bigLamExtension
             \*/ kind
             \*/ (spaceAllowed */ sub exprI))

-- | The function application constructor takes the form:
--
-- AT EXPR EXPR
appExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
appExpr = label (enhancingLabel "Application") $
  at */
  (appIso \$/ appExtension
          \*/ (spaceAllowed  */ sub exprI)
          \*/ (spaceRequired */ sub exprI))

-- | The Big function application constructor takes the form:
--
-- BIGAT EXPR TYPE
bigAppExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
bigAppExpr = label (enhancingLabel "Big application") $
  bigAt */
  (bigAppIso \$/ bigAppExtension
             \*/ (spaceAllowed  */ sub exprI)
             \*/ (spaceRequired */ sub typI))

-- | The binding constructor is some form of reference to a value bound by a
-- lambda abstraction. It is likely to be an index of name.
--
-- It defers to the implicit binding grammar.
bindingExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
bindingExpr = label (enhancingLabel "Binding") $
  bindingIso \$/ bindingExtension
             \*/ binding

-- | The content binding constructor is a name which references an expression by
-- it's content.
--
-- It defers to the implicit content binding grammar.
contentBindingExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
contentBindingExpr = label (enhancingLabel "Content binding") $
  contentBindingIso \$/ contentBindingExtension
                    \*/ contentBinding

-- | The Sum constructor takes the form:
--
-- PLUS natural EXPR TYPE+
sumExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
sumExpr = label (enhancingLabel "Sum") $
  plus */
  (sumIso \$/ sumExtension
          \*/ sumIndex
          \*/ (spaceAllowed */ sub exprI)
          \*/ (spaceRequired */ (sepBy1 spacePreferred $ sub typI)))

sumIndex :: Grammar Int
sumIndex = label (enhancingLabel "Type index") $
  token natural

-- | The Product constructor takes the form:
--
-- STAR EXPR*
productExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
productExpr = label (enhancingLabel "Product")
  star */
  (productIso \$/ productExtension \*/ (spaceAllowed */ sepBy spacePreferred (sub exprI)))

-- | The Union constructor takes the form:
--
-- UNION TYPE EXPR TYPE*
unionExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
unionExpr = label (enhancingLabel "Union") $
  union */
  (unionIso \$/ unionExtension
            \*/ (spaceAllowed   */ sub typI)
            \*/ (spacePreferred */ sub exprI)
            \*/ (setIso \$/ (spacePreferred */ sepBy spacePreferred (sub typI))))

-- | The Case constructor takes the form:
--
-- "CASE" CASEBODY
--
-- The CASEBODY contains:
-- - A Scrutinee expression
-- - Either:
--   - One or many branches and an optional default expression
--   - A default expression
caseAnalysisExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
caseAnalysisExpr = label (enhancingLabel "Case") $
  textIs "CASE" */
  (caseIso \$/ caseAnalysisExtension \*/ (spaceRequired */ caseBody (sub exprI)))
  where
    caseIso :: Iso (CaseAnalysisExtension phase, Case (ExprFor phase) (PatternFor phase)) (ExprFor phase)
    caseIso = Iso
      {_forwards  = \(ext,c) -> Just . CaseAnalysisExt ext $ c
      ,_backwards = \e -> case e of
         CaseAnalysisExt ext c
           -> Just (ext, c)
         _ -> Nothing
      }

-- | Defer to the implicit Grammar for expr extensions.
exprExtensionExpr
  :: ( Implicits phase
     , Constraints phase
     )
  => Grammar (ExprFor phase)
exprExtensionExpr = exprExtensionIso \$/ exprExtension

-- | A Commented Expression as the expr extension constructor takes the form:
--
-- '"' COMMENT '"' EXPR
commentedExpr
  :: (Constraints phase)
  => GrammarDependencies phase
  -> TypeGrammarDependencies phase
  -> PatternGrammarDependencies phase
  -> Grammar (Commented (ExprFor phase))
commentedExpr eDep tDep pDep =
  charIs '"' */
  (commentedExprIso \$/ (commentText \* charIs '"')
                    \*/ (spaceAllowed */ sub (expr eDep tDep pDep))
  )
  where
    commentText :: Grammar Comment
    commentText = commentTextIso \$/ longestMatching isCommentChar

    -- TODO: Use a better character class here.
    isCommentChar :: Char -> Bool
    isCommentChar c = c /= '\"'

    commentTextIso :: Iso Text Comment
    commentTextIso = Iso
      {_forwards  = Just . Comment
      ,_backwards = \(Comment t) -> Just t
      }

-- | Parse an expression given grammar dependencies for things such as:
-- - Expression bindings    (E.G. var)
-- - Expression abstraction (E.G. typ)
-- - Type bindings          (E.G. tyVar)
--
-- and a level signifier.
--
-- Top-level expressions prefer not to be surrounded by parenthesis.
--
-- Nested sub-expressions will prefer to be surrounded by parenthesis unless the
-- specific sub-expression is unambiguous (like a single integer variable
-- binding)
expr
  :: Constraints phase
  => GrammarDependencies phase
  -> TypeGrammarDependencies phase
  -> PatternGrammarDependencies phase
  -> Level
  -> Grammar (ExprFor phase)
expr grammarDependencies typeGrammarDependencies patternGrammarDependencies exprLevel =
  let ?grammarDependencies        = grammarDependencies
      ?typeGrammarDependencies    = typeGrammarDependencies
      ?patternGrammarDependencies = patternGrammarDependencies
   in exprI exprLevel

exprI
  :: forall phase
   . ( Implicits phase
     , Constraints phase
     )
  => Level
  -> Grammar (ExprFor phase)
exprI = level unambiguousExprI ambiguousExprI
  where
    unambiguousExprI :: [Grammar (ExprFor phase)]
    unambiguousExprI =
      [ bindingExpr
      , contentBindingExpr
      , exprExtensionExpr
      ]

    ambiguousExprI :: [Grammar (ExprFor phase)]
    ambiguousExprI =
      [ lamExpr
      , bigLamExpr
      , appExpr
      , bigAppExpr
      , sumExpr
      , productExpr
      , unionExpr
      , caseAnalysisExpr
      ]


{- Misc -}

noExtG :: Grammar NoExt
noExtG = rpure noExt -- This wont work?


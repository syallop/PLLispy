{-# LANGUAGE MultiWayIf, OverloadedStrings, GADTs #-}
module PLLispy.Pattern.Iso
  ( sumPatternIso
  , productPatternIso
  , unionPatternIso
  , bindingPatternIso
  , bindIso

  , patternExtensionIso
  , commentedPatternIso
  )
  where

import PLGrammar
import Reversible.Iso

import PL.Case
import PL.Commented
import PL.Expr
import PL.Kind
import PL.Name
import PL.Pattern
import PL.TyVar
import PL.Type
import PL.Var

import Data.Char
import Data.List.NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as Text

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
-}

sumPatternIso :: Iso (SumPatternExtension phase, (Int, PatternFor phase)) (PatternFor phase)
sumPatternIso = Iso
  {_forwards = \(ext, (ix, pattern)) -> Just $ SumPatternExt ext ix pattern
  ,_backwards = \pattern -> case pattern of
                              SumPatternExt ext ix pattern
                                -> Just (ext, (ix, pattern))
                              _ -> Nothing
  }

productPatternIso :: Iso (ProductPatternExtension phase, [PatternFor phase]) (PatternFor phase)
productPatternIso = Iso
  {_forwards = \(ext, patterns) -> Just $ ProductPatternExt ext patterns
  ,_backwards = \pattern -> case pattern of
                              ProductPatternExt ext patterns
                                -> Just (ext, patterns)
                              _ -> Nothing
  }

unionPatternIso :: Iso (UnionPatternExtension phase, (TypeFor phase, PatternFor phase)) (PatternFor phase)
unionPatternIso = Iso
  {_forwards = \(ext, (tyIx, pattern)) -> Just $ UnionPatternExt ext tyIx pattern
  ,_backwards = \pattern -> case pattern of
                              UnionPatternExt ext tyIx pattern
                                -> Just (ext, (tyIx, pattern))
                              _ -> Nothing
  }

bindingPatternIso :: Iso (BindingPatternExtension phase, BindingFor phase) (PatternFor phase)
bindingPatternIso = Iso
  {_forwards = \(ext,b) -> Just $ BindingPatternExt ext b
  ,_backwards = \pattern -> case pattern of
                              BindingPatternExt ext b
                                -> Just (ext,b)
                              _ -> Nothing
  }

bindIso :: Iso (BindExtension phase) (PatternFor phase)
bindIso = Iso
  {_forwards = \ext -> Just $ BindExt ext
  ,_backwards = \pattern -> case pattern of
                              BindExt ext
                                -> Just ext
                              _ -> Nothing
  }

patternExtensionIso :: Iso (PatternExtension phase) (PatternFor phase)
patternExtensionIso = Iso
  {_forwards = \ext -> Just $ PatternExtensionExt ext
  ,_backwards = \pattern -> case pattern of
                              PatternExtensionExt ext
                                -> Just ext
                              _ -> Nothing
  }

commentedPatternIso :: Iso (Comment, PatternFor phase) (Commented (PatternFor phase))
commentedPatternIso = Iso
  {_forwards = \(c,p)
                -> Just . Commented c $ p
  ,_backwards = \(Commented c p) -> Just (c, p)
  }


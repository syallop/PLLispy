{-# LANGUAGE MultiWayIf, OverloadedStrings, GADTs, LambdaCase #-}
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

import Reversible.Iso

import PL.Commented
import PL.Expr
import PL.Pattern
import PL.Type

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
-}

sumPatternIso :: Iso (SumPatternExtension phase, (Int, PatternFor phase)) (PatternFor phase)
sumPatternIso = Iso
  {_forwards = \(ext, (ix, pattern)) -> Just $ SumPatternExt ext ix pattern
  ,_backwards = \case
                  SumPatternExt ext ix pattern
                    -> Just (ext, (ix, pattern))
                  _ -> Nothing
  }

productPatternIso :: Iso (ProductPatternExtension phase, [PatternFor phase]) (PatternFor phase)
productPatternIso = Iso
  {_forwards = \(ext, patterns) -> Just $ ProductPatternExt ext patterns
  ,_backwards = \case
                  ProductPatternExt ext patterns
                    -> Just (ext, patterns)
                  _ -> Nothing
  }

unionPatternIso :: Iso (UnionPatternExtension phase, (TypeFor phase, PatternFor phase)) (PatternFor phase)
unionPatternIso = Iso
  {_forwards = \(ext, (tyIx, pattern)) -> Just $ UnionPatternExt ext tyIx pattern
  ,_backwards = \case
                  UnionPatternExt ext tyIx pattern
                    -> Just (ext, (tyIx, pattern))
                  _ -> Nothing
  }

bindingPatternIso :: Iso (BindingPatternExtension phase, BindingFor phase) (PatternFor phase)
bindingPatternIso = Iso
  {_forwards = \(ext,b) -> Just $ BindingPatternExt ext b
  ,_backwards = \case
                  BindingPatternExt ext b
                    -> Just (ext,b)
                  _ -> Nothing
  }

bindIso :: Iso (BindExtension phase) (PatternFor phase)
bindIso = Iso
  {_forwards = \ext -> Just $ BindExt ext
  ,_backwards = \case
                  BindExt ext
                    -> Just ext
                  _ -> Nothing
  }

patternExtensionIso :: Iso (PatternExtension phase) (PatternFor phase)
patternExtensionIso = Iso
  {_forwards = \ext -> Just $ PatternExtensionExt ext
  ,_backwards = \case
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


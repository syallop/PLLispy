{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PLLispy.PatternIso where

import PLGrammar
import Reversible.Iso

import PL.Case
import PL.Expr
import PL.Type
import PL.Kind
import PL.TyVar
import PL.Name
import PL.Var
import PL.Commented
import PL.Pattern

import qualified Data.Set as Set
import qualified Data.Text as Text

import Data.Char
import Data.List.NonEmpty

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -
 - We _could_ make these more generic and use 'PatternFor' and the extension
 - types.
 -}

sumPatternIso :: Iso (Int, CommentedPattern) CommentedPattern
sumPatternIso = Iso
  {_forwards = \(ix, pattern) -> Just $ SumPattern ix pattern
  ,_backwards = \pattern -> case pattern of
                              SumPattern ix pattern
                                -> Just (ix, pattern)
                              _ -> Nothing
  }

productPatternIso :: Iso [CommentedPattern] CommentedPattern
productPatternIso = Iso
  {_forwards = \patterns -> Just $ ProductPattern patterns
  ,_backwards = \pattern -> case pattern of
                              ProductPattern patterns
                                -> Just patterns
                              _ -> Nothing
  }

unionPatternIso :: Iso (CommentedType, CommentedPattern) CommentedPattern
unionPatternIso = Iso
  {_forwards = \(tyIx, pattern) -> Just $ UnionPattern tyIx pattern
  ,_backwards = \pattern -> case pattern of
                              UnionPattern tyIx pattern
                                -> Just (tyIx, pattern)
                              _ -> Nothing
  }

bindingPatternIso :: Iso Var CommentedPattern
bindingPatternIso = Iso
  {_forwards = \b -> Just $ BindingPattern b
  ,_backwards = \pattern -> case pattern of
                              BindingPattern b
                                -> Just b
                              _ -> Nothing
  }

bindIso :: Iso () CommentedPattern
bindIso = Iso
  {_forwards = \() -> Just Bind
  ,_backwards = \pattern -> case pattern of
                              Bind
                                -> Just ()
                              _ -> Nothing
  }

commentedIso :: Iso (Comment, CommentedPattern) CommentedPattern
commentedIso = Iso
  {_forwards = \(c,pattern)
                -> Just . CommentedPattern c $ pattern
  ,_backwards = \pattern
                 -> case pattern of
                      CommentedPattern c pattern
                        -> Just (c, pattern)
                      _ -> Nothing
  }


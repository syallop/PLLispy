{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
module PLLispy.Kind.Iso where

import Reversible.Iso

import PL.Kind

{- Iso's that map between constructors and their contained values
 - These can/ should be mechanically created, perhaps with TH/ Generics.
 -}

kindIso :: Iso () Kind
kindIso = Iso
  {_forwards = \() -> Just Kind
  ,_backwards = \Kind -> Just ()
  }

kindArrowIso :: Iso (Kind,Kind) Kind
kindArrowIso = Iso
  {_forwards = \(fromKy, toKy) -> Just $ KindArrow fromKy toKy
  ,_backwards = \kind -> case kind of
                          KindArrow fromKy toKy
                            -> Just (fromKy, toKy)

                          _ -> Nothing
  }


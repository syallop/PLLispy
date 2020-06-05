{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : PLLispy.Name
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Collection of name-like things and related functions. I.E. Hashes and variables.
-}
module PLLispy.Name
  ( var
  , contentNameGrammar

  , base58Hash
  , base58HashIso

  , hashAlgorithmGrammar
  , sha512

  , base58Text

  , shortHash
  )
  where

import PL.Name
import PL.Hash
import PL.Var
import PL.HashStore

import PLGrammar
import Reversible
import Reversible.Iso

import Data.Text (Text)
import qualified Data.Text as Text

-- | A Content name is a full, unambiguous hash represented in base58.
-- E.G.
--
-- #SHA512/BGpbCZNFqE6aQE1pb9GvP195dE6qFHsSPUZVqpBruZUWzZQZSChvBoXEmei4RZ2yYkQ61ufMh51s3XEeMBGmHCAmW434GdHxiQNY19GLKHvFh9TKL8yhRs6yXC5rWgBDoT7dFA6nBpKi2E31PRct8Sv8gxBfrXs1C85BpgkB7iHkXAW
--
-- Short hashes are not understood. The leading algorithm identifier may be
-- upper or lower case.
contentNameGrammar
  :: Grammar ContentName
contentNameGrammar = contentNameIso \$/ charIs '#' */ base58Hash
  where
    contentNameIso :: Iso Hash ContentName
    contentNameIso = Iso
      { _forwards = Just . mkContentName . HashIs
      , _backwards = Just . contentName
      }

-- | A Hash is an algorithm identifier a slash separator and a string of base58
-- text.
base58Hash :: Grammar Hash
base58Hash = base58HashIso \$/ (hashAlgorithmGrammar \* charIs '/') \*/ base58Text

base58HashIso :: Iso (HashAlgorithm, Text) Hash
base58HashIso = Iso
  {_forwards  = \(alg,bytes) -> mkBase58 alg bytes
  ,_backwards = Just . unBase58
  }

-- | Known hash algorithm identifiers.
hashAlgorithmGrammar :: Grammar HashAlgorithm
hashAlgorithmGrammar = sha512

-- | sha512 may be spelled lower or upper case
sha512 :: Grammar HashAlgorithm
sha512 = (textIs "SHA512" \|/ textIs "sha512") */ rpure SHA512

-- | The longest string of base58 characters
base58Text :: Grammar Text
base58Text = longestMatching isBase58

isBase58 :: Char -> Bool
isBase58 = (`elem` base58Characters)

base58Characters :: [Char]
base58Characters = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- #abcd
-- #SHA512/abcd
-- #SHA512/abcdefghij....

-- | A ShortHash is an abreviation for a Hash.
-- - The algorithm may be omitted when it is the default.
-- - The base58 encoded bytes may be truncated.
--
-- E.G. A full hash of:
-- #SHA512/BGpbCZNFqE6aQE1pb9GvP195dE6qFHsSPUZVqpBruZUWzZQZSChvBoXEmei4RZ2yYkQ61ufMh51s3XEeMBGmHCAmW434GdHxiQNY19GLKHvFh9TKL8yhRs6yXC5rWgBDoT7dFA6nBpKi2E31PRct8Sv8gxBfrXs1C85BpgkB7iHkXAW
--
-- Could instead be written:
-- #BGpbCZNFqE6aQE1pb9GvP195dE6qFHsSPUZVqpBruZUWzZQZSChvBoXEmei4RZ2yYkQ61ufMh51s3XEeMBGmHCAmW434GdHxiQNY19GLKHvFh9TKL8yhRs6yXC5rWgBDoT7dFA6nBpKi2E31PRct8Sv8gxBfrXs1C85BpgkB7iHkXAW
-- #SHA512/BGpbCZN
-- #BGpbCZN
shortHash :: Grammar ShortHash
shortHash = alternatives
  [ base58ShortHashIso \$/ (rpure Nothing)
                       \*/ base58Text

  , base58ShortHashIso \$/ (justIso \$/ (hashAlgorithmGrammar \* charIs '/'))
                       \*/ base58Text
  ]
  where
    justIso :: Iso a (Maybe a)
    justIso = Iso
      {_forwards  = Just . Just
      ,_backwards = id
      }

base58ShortHashIso :: Iso (Maybe HashAlgorithm, Text) ShortHash
base58ShortHashIso = Iso
  {_forwards  = \(mAlg,bytes) -> mkBase58ShortHash mAlg bytes
  ,_backwards = Just . unBase58ShortHash
  }
-- | Var can be used as an expressions binding.
--
-- It refers to a bound value by counting back to the lambda which
-- abstracted it. It takes the form of a natural number E.G:
--
-- 0,1,2,...
var
  :: Grammar Var
var =
  varIso \$/ natural -- A variable is given by a natural number.
  where
    varIso :: Iso Int Var
    varIso = Iso
      {_forwards = Just . mkVar
      ,_backwards = Just . fromEnum -- TODO: Partial
      }


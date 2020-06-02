module PLLispy.Name
  ( contentNameGrammar
  , var
  )
  where

import PL.Name
import PL.Hash
import PL.Var

import PLGrammar
import Reversible
import Reversible.Iso

import Data.Text (Text)
import qualified Data.Text as Text

-- A Content name is a full, unambiguous hash represented in base58.
-- E.G.
--
-- #SHA512/BGpbCZNFqE6aQE1pb9GvP195dE6qFHsSPUZVqpBruZUWzZQZSChvBoXEmei4RZ2yYkQ61ufMh51s3XEeMBGmHCAmW434GdHxiQNY19GLKHvFh9TKL8yhRs6yXC5rWgBDoT7dFA6nBpKi2E31PRct8Sv8gxBfrXs1C85BpgkB7iHkXAW
--
-- Short hashes are not understood. The leading algorithm identifier is required
-- in exact case.
contentNameGrammar
  :: Grammar ContentName
contentNameGrammar = contentNameIso \$/ (charIs '#' */ hashGrammar)
  -- TODO: Define Hash Grammar rather than delegating to 'readBase58' to support
  -- better error messages.
  where
    contentNameIso :: Iso Hash ContentName
    contentNameIso = Iso
      { _forwards = Just . mkContentName . HashIs
      , _backwards = Just . contentName
      }

    hashGrammar
      :: Grammar Hash
    hashGrammar = hashIso \$/ longestMatching (`elem` hashCharacters)

    hashIso :: Iso Text Hash
    hashIso = Iso
      { _forwards  = readBase58
      , _backwards = Just . showBase58
      }

    -- A slash used to separate the algorithm from a Base58 charset (deliberately excludes 0).
    hashCharacters = ['/'] <> ['1'..'9'] <> ['A'..'Z'] <> ['a'..'z']

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


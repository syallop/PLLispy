module PLLispy.Name
  ( contentNameGrammar
  )
  where

import PL.Name
import PL.Hash

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


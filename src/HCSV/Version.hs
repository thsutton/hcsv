module HCSV.Version ( version ) where

import           Data.List
import           Data.Version (Version(..))

import qualified Paths_hcsv as P

-- | Turn the Cabal-generated Version into a String of the normal form.
version :: String
version = number++(if null tag then "" else "-" ++ tag)
  where
    number = concat $ intersperse "." $ map show $ versionBranch P.version
    tag = concat $ intersperse "-" $ versionTags P.version

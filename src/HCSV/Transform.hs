{-# LANGUAGE OverloadedStrings #-}
module HCSV.Transform where

import           Data.ByteString.Char8 ()
import qualified Data.ByteString as BS
import           HCSV.Types

-- | Apply a list of field selections to a CSV record.
selectCSV :: Maybe [CSVFieldS] -> Record -> Record
selectCSV Nothing rec = rec
selectCSV (Just _) rec = rec

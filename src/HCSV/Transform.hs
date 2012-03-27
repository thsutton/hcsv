{-# LANGUAGE OverloadedStrings #-}
module HCSV.Transform where

import           Data.ByteString.Char8 ()
import qualified Data.ByteString as BS
import           Data.List
import           Data.Maybe
import           HCSV.Types

-- | Apply a list of field selections to a CSV record.
selectCSV :: Maybe [CSVFieldS] -> Record -> Record
selectCSV Nothing rec = rec
selectCSV (Just fs) rec = let fields = zip [1..] rec in select fs fields
  where
    select [] _ = []
    select (n:ns) d = let hd = fromMaybe "" (lookup n d)
                      in hd:(select ns d)

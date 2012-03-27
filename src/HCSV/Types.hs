module HCSV.Types where

import qualified Data.ByteString as BS


-- | A series of records.
type CSV = [Record]

-- | A record is a list of fields
-- 
-- All records in a file should be the same length, but there's not enough 
-- utility to justify the hassle of implementing this requirement.
type Record = [Field]

-- | A field is a ByteString
type Field = BS.ByteString

-- | A selector
type CSVFieldS = Int
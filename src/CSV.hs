{-# LANGUAGE OverloadedStrings #-}
module CSV where
  

-- ( fieldParser
-- , recordParser
-- ) where

import           Prelude hiding (takeWhile)
import qualified Data.ByteString as BS
import           Control.Applicative ((<|>), (<*), (*>), many)
import           Data.Attoparsec
import           Data.Attoparsec.Combinator
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Word

-- | A series of records.
type CSV = [Record]

-- | A record is a list of fields
-- 
-- All records in a file should be the same length, but there's not enough 
-- utility to justify the hassle of implementing this requirement.
type Record = [Field]

-- | A field is a ByteString
type Field = BS.ByteString

quote :: Word8
quote = 34

apos :: Word8
apos = 44

-- | Parse a CSV records.
recordParser :: Parser Record
recordParser = fieldParser `sepBy` (word8 apos)

-- | Parse a CSV field (quoted or unquoted).
fieldParser :: Parser Field
fieldParser = (quotedField <|> unquotedField)

-- | Parse an unquoted field.
unquotedField :: Parser Field
unquotedField = takeWhile (notInClass ",\n\r\"")

-- | Parse a quoted field.
-- 
-- XXX TODO: Make this suck less.
quotedField :: Parser Field
quotedField = (word8 quote) *> (content) <* (word8 quote)
  where qs = word8 quote *> word8 quote
        content = do
          ws <- many (notWord8 quote <|> qs)
          return $ BS.pack ws


recordConduit :: (MonadThrow m) => Sink BS.ByteString m Record
recordConduit = sinkParser recordParser
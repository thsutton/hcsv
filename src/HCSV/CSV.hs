{-# LANGUAGE OverloadedStrings #-}
module HCSV.CSV where

import           Prelude hiding (takeWhile)
import qualified Data.ByteString as BS
import           Control.Applicative ((<|>), (<*), (*>), many)
import           Data.Attoparsec
import           Data.Attoparsec.Combinator
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Word

import           HCSV.Options
import           HCSV.Types

quote :: Word8
quote = 34

comma :: Word8
comma = 44

-- | Parse a CSV records.
recordParser :: Parser Record
recordParser = (fieldParser `sepBy` (word8 comma)) <* (takeWhile $ inClass "\r\n")

-- | Parse a CSV field (quoted or unquoted).
fieldParser :: Parser Field
fieldParser = try (quotedField <|> unquotedField)

-- | Parse an unquoted field.
unquotedField :: Parser Field
unquotedField = takeWhile (notInClass ",\n\r\"")

-- | Parse a quoted field.
-- 
-- XXX TODO: Make this suck less. See issue #1
quotedField :: Parser Field
quotedField = (word8 quote) *> (content) <* (word8 quote)
  where qs = word8 quote *> word8 quote
        content = do
          ws <- many (notWord8 quote <|> qs)
          return $ BS.pack ws

-- | A conduit Sink to parse CSV records.
recordSink :: (MonadThrow m) => Sink BS.ByteString m Record
recordSink = sinkParser recordParser

-- | Convert a Record for output.
recordText :: HCSVOptions -> Record -> BS.ByteString
recordText opt r = (BS.intercalate "," fields) `BS.append` "\r\n"
  where fields = map (escapeField' opt) r

-- | Quote a field if requested or required.
escapeField' :: HCSVOptions -> Field -> Field
escapeField' opt f = let quote = (optQuoteAll opt) || BS.any (inClass ",\n\r\"") f
                    in if quote then BS.concat ["\"", escapeField f, "\""] else f

-- | Escape quotes within fields.
escapeField :: Field -> Field
escapeField f = BS.intercalate "\"\"" $ BS.split quote f

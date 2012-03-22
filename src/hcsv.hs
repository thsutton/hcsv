module Main where

import           Prelude hiding (sequence)
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           System.FilePath
import           System.IO

import qualified CSV

main :: IO ()
main = do
  runResourceT $
         (sourceHandle stdin $= sequence CSV.recordSink) $= CL.map CSV.recordText $$ sinkHandle stdout

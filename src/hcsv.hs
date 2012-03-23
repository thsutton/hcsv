module Main where

import           Prelude hiding (sequence)
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           System (getArgs)
import           System.Console.GetOpt
import           System.Exit (exitSuccess)
import           System.FilePath
import           System.IO

import           HCSV.Options
import qualified HCSV.CSV as CSV

main :: IO ()
main = do
  -- Process command-line options
  args <- getArgs
  opts <- case getOpt RequireOrder options args of
    (actions, [],      [])     -> foldl (>>=) (return defaultOptions) actions
    (_,       nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,       _,       msgs)   -> error $ concat msgs ++ usageInfo "hcsv" options

  runResourceT $
         (sourceHandle (optInput opts) $= sequence CSV.recordSink) $= CL.map (CSV.recordText opts) $$ sinkHandle (optOutput opts)

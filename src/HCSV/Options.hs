module HCSV.Options where

import           System.Console.GetOpt
import           System.Exit
import           System.IO

import           HCSV.Version ( version )

data HCSVOptions = Options
  { optName     :: String         -- ^ Name of input.
  , optInput    :: Handle         -- ^ Handle to read from.
  , optOutput   :: Handle         -- ^ Handle to write to.
  , optError    :: Maybe Handle   -- ^ Handle to write errors to (or don't).
  , optQuoteAll :: Bool           -- ^ Quote all fields in output.
  , optFields   :: Maybe [Int]    -- ^ Fields to output (or all).
  }


-- | The default options
--
-- By default we read and write to the console and quote everything.
defaultOptions :: HCSVOptions
defaultOptions = Options
  { optName = "-"
  , optInput = stdin
  , optOutput = stdout
  , optError = Just stderr
  , optQuoteAll = False
  , optFields = Nothing
  }


-- | Options for use by getOpt.
options :: [OptDescr (HCSVOptions -> IO HCSVOptions)]
options = 
  [ Option ['V'] ["version"] (NoArg  showVersion)    "show version number"
  , Option ['h'] ["help"]    (NoArg  showHelp) "show the help"
  , Option ['f'] ["field"]   (ReqArg fieldNumber "NUMBER") "Fields to output"
  , Option ['i'] ["input"]   (ReqArg readInput "FILE") "Read from FILE"
  , Option ['o'] ["output"]  (ReqArg writeOutput "FILE") "Write to FILE"
  , Option ['q'] ["quote"]   (NoArg  setQuote) "Quote all fields"
  ]
--  , Option ['e'] ["error"]   (OptArg writeErrors "FILE") "Write errors to FILE"


-- | Show the version number and terminate.
showVersion :: HCSVOptions -> IO HCSVOptions
showVersion _ = do
  putStrLn $ "hcsv " ++ version
  exitWith ExitSuccess

-- | Show the help message and terminate.
showHelp :: HCSVOptions -> IO HCSVOptions
showHelp _ = do
  putStrLn $ usageInfo "hcsv" options
  exitWith ExitSuccess

-- | Open and remember the handle to read input from.
readInput :: FilePath -> HCSVOptions -> IO HCSVOptions
readInput arg opt = do
  putStrLn $ "# Using " ++ arg ++ " for input."
  hand <- openFile arg ReadMode
  hSetBuffering hand NoBuffering
  return opt { optInput = hand, optName = arg }


-- | Open and remember the handle to write output to.
writeOutput :: FilePath -> HCSVOptions -> IO HCSVOptions
writeOutput arg opt = do
  putStrLn $ "# Using " ++ arg ++ " for output."
  hand <- openFile arg WriteMode
  hSetBuffering hand NoBuffering
  return opt { optOutput = hand }


-- | Open and remember the handle to record errors to.
writeErrors :: Maybe FilePath -> HCSVOptions -> IO HCSVOptions
writeErrors arg opt = do
  (handle, fname) <- maybe
    (return (stderr, "<stderr>"))
    (\fp -> openFile fp WriteMode >>= \h -> return (h, fp))
    arg
  putStrLn $ "# Using " ++ fname ++ " for errors."
  hSetBuffering handle NoBuffering
  return opt { optError = Just handle }


-- | Check the fields to output and remember them.
fieldNumber :: String -> HCSVOptions -> IO HCSVOptions
fieldNumber arg opt = do
  let field = read arg
  if (field < 1)
    then fail "Field number must be greater than 0!"
    else return ()
  return $ opt { optFields = Just [] }


-- | Configure to quote all fields.
setQuote :: HCSVOptions -> IO HCSVOptions
setQuote opt = do
  return $ opt { optQuoteAll = True }

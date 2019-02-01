{-# LANGUAGE LambdaCase #-}
import Flags
import Converter
import qualified Data.Text.IO  as TIO
import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Error.Safe (tryRight)
import Control.Monad.Trans.Class (lift)

-- ============================== MAIN ======================================
main :: IO ()
main = runExceptT compute >>= \case
    Left str -> hPutStrLn stderr str >> usage
    Right () -> return ()

compute :: ExceptT String IO ()
compute = do
     -- Try to get the flags
    args <- lift getArgs
    flags <- tryRight $ parseAllArgsIntoFlags args

     -- If Help flag is set, just print usage
    when (Help `elem` flags) 
        $ throwE "Printing usage."

    -- Try parse input from flags
    inp <- lift $ extractInput flags
    n <- tryRight inp

    -- If Verbose flag is set, print resulting parsed number
    when (Verbose `elem` flags) 
        $ lift $ hPutStrLn stderr $ "Parsed number: " ++ show n

    -- Print number converted to Conway-Wechsler form
    lift $ TIO.putStrLn $ convert flags n

-- Print usage
usage :: IO ()
usage = mapM_ (hPutStrLn stderr) ls
  where
  ls :: [String]
  ls = 
    ["Usage: conway-wechsler [flags]"
    ,"  INPUT"
    ,"  <n>: a number composed of digits, or a mathematical expression"
    ,"       For more information on valid mathematical expressions, go to"
    ,"       https://github.com/dylan-thinnes/conway-wechsler#mathematical-expressions"
    ,"    -: read number composed of digits from stdin,"
    ,"       If number/expression is unspecified, this is default"
    ,"  OUTPUT"
    ,"   --newline,"
    ,"   -n: newline between each -illion"
    ,"   --keep,"
    ,"   -k: express numbers < 1000 as numerals, not words"
    ,"       also, write 'negative' as '-'"
    ,"   --verbose,"
    ,"   -v: print verbose status updates"
    ,"  MISCELLANEOUS"
    ,"   --help,"
    ,"   -h: show usage page"
    ]

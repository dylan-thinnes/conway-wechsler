module Flags where

import qualified MathParse as MP
import Data.Char (isAlpha)
import Data.Maybe (maybe)
import Data.List (intersperse)
import Control.Arrow (left)

-- ============================== FLAGS =====================================
-- Express all possible flags that can be passed to the command line
data Flag = Newline 
          | KeepNumerals 
          | Verbose
          | Help
          | Stdin
          | Input String
          deriving (Eq, Show)

isInput :: Flag -> Bool
isInput (Input e) = True
isInput Stdin     = True
isInput _         = False

-- Try parse 0 to many flags from multiple command line arguments
parseAllArgsIntoFlags :: [String] -> Either String [Flag]
parseAllArgsIntoFlags [] = Right []
parseAllArgsIntoFlags args = concat <$> mapM parseArgIntoFlags args

-- Try parse 0 to many flags from a single command line argument
parseArgIntoFlags :: String -> Either String [Flag]
parseArgIntoFlags "-"         = Right [Stdin]
parseArgIntoFlags ('-':'-':l) = (:[]) <$> parseFlagFromLonghand l
parseArgIntoFlags x 
  | isAShorthandFlagList x 
  = mapM parseFlagFromShorthand $ tail x
  | otherwise
  = Right [Input x]

-- Checks if a string could be a shorthand flag list (e.g. -hk), and not an
-- integer / mathematical expression
isAShorthandFlagList :: String -> Bool
isAShorthandFlagList ('-':ss) = all isAlpha ss
isAShorthandFlagList _        = False

-- Try to obtain a flag from a single character
getFlagFromShorthand :: Char   -> Maybe Flag
getFlagFromShorthand 'k' = Just KeepNumerals
getFlagFromShorthand 'n' = Just Newline
getFlagFromShorthand 'h' = Just Help
getFlagFromShorthand 'v' = Just Verbose
getFlagFromShorthand _   = Nothing

-- Try to obtain a flag from a string
getFlagFromLonghand  :: String -> Maybe Flag
getFlagFromLonghand "keep"       = Just KeepNumerals
getFlagFromLonghand "newline"    = Just Newline
getFlagFromLonghand "help"       = Just Help
getFlagFromLonghand "verbose"    = Just Verbose
getFlagFromLonghand _            = Nothing

-- Change unfound flags into error messages
maybeToFlagError :: (Show a) => a -> Maybe Flag -> Either String Flag
maybeToFlagError s = maybeToEither $ "Invalid flag " ++ show s

-- Combine maybeToFlagError and getFlagFromShorthand
parseFlagFromShorthand :: Char -> Either String Flag
parseFlagFromShorthand c = maybeToFlagError c $ getFlagFromShorthand c

-- Combine maybeToFlagError and getFlagFromLonghand
parseFlagFromLonghand :: String -> Either String Flag
parseFlagFromLonghand s = maybeToFlagError s $ getFlagFromLonghand s

-- Try and find an input flag, and act on it to produce an input Integer
extractInput :: Bool -> [Flag] -> IO (Either String Integer)
extractInput safe flags = case getInputFromFlags flags of
                            Left x  -> return $ Left x
                            Right f -> tryGetInput safe f

getInputFromFlags :: [Flag] -> Either String Flag
getInputFromFlags flags
  | length inputFlags == 1 = Right $ head inputFlags
  | length inputFlags == 0 = Right $ Stdin
  | otherwise              = multipleInputFlagsError inputFlags
    where
    inputFlags = filter isInput flags

multipleInputFlagsError :: [Flag] -> Either String a
multipleInputFlagsError fs = Left
                           $ ("Two or more arguments specify an input, " ++)
                           $ concat $ intersperse " and "
                           $ map showInput fs
    where
    showInput :: Flag -> String
    showInput (Input i) = "'" ++ i ++ "'"
    showInput Stdin     = "'-' (stdin)"
    showInput _         = error "Non-input flag supplied to showInput"

-- | Try and turn one of two input flags, Stdin or Input n, into a meaningful
-- Integer value
-- * Stdin means try read from stdin
-- * Input i means just return i
tryGetInput :: Bool -> Flag -> IO (Either String Integer)
tryGetInput safe Stdin     = (Input <$> getLine) >>= tryGetInput safe
tryGetInput safe (Input s) = return $ tryExprToInt safe =<< tryStrToExpr s

-- | Try to turn a string into an expression
tryStrToExpr :: String -> Either String MP.Expr
tryStrToExpr = left show . MP.parseToExpr

-- | Try to turn an expression into an Integer, with safety flag
tryExprToInt :: Bool -> MP.Expr -> Either String Integer
tryExprToInt safe = left show . MP.runReduce (MP.reduceSafe constraints)
    where
    constraints :: [MP.Constraint Integer]
    constraints = if not safe then [] else
      [ MP.Constraint
          { MP.conds = MP.BinaryExprF MP.Exponentiate (>100) (>100000)
          , MP.err   = MP.TooLarge
          }
      , MP.Constraint 
          { MP.conds = MP.BinaryExprF MP.Exponentiate (const True) (>1000000)
          , MP.err   = MP.TooLarge
          }
      , MP.Constraint 
          { MP.conds = MP.UnaryExprF MP.Factorial (>1000)
          , MP.err   = MP.TooLarge
          }
      ]

-- | Map maybe to an Either value, supplying the default Left value
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

-- | Try to parse a string into an integer
tryParseInt :: String -> Maybe Integer
tryParseInt str = let attempt = reads str in
                  if null attempt || (not $ null $ snd $ head attempt)
                  then Nothing
                  else Just $ fst $ attempt !! 0


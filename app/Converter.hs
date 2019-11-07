{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImplicitParams #-}
module Converter where

import Flags
import qualified Data.Sequence as S
import qualified Data.Text     as T
import Data.Word (Word8)
import Control.Monad (mzero, liftM, join)
import Data.Foldable (toList)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)

import Debug.Trace

-- ============================ DATATYPES ===================================
-- | Data type for expressing results of parsing
type Fields = S.Seq T.Text

-- | Extract the concatenation of a group of fields
extract :: Fields -> T.Text
extract = T.concat . toList . collapse

-- | Run a function on a group of fields if the fields are not empty
runIfNotNull :: (Fields -> Fields) -> Fields -> Fields
runIfNotNull func fields = if fields == mzero
                           then fields
                           else func fields

-- | Collapse a group of fields into a single field
collapse :: Fields -> Fields
collapse = runIfNotNull (pure . T.concat . toList)

-- | Put spaces between each parsed result
wordify :: Fields -> Fields
wordify = S.intersperse " "

-- | Put newlines between each parsed result
lineify :: Fields -> Fields
lineify = S.intersperse "\n"

-- | Data type for storing & unstoring groups of numbers by three
-- This will help in inflecting both normal numbers and prefixes for zillions
data Triple = Triple {
                hun :: Word8,
                ten :: Word8,
                one :: Word8
              }
    deriving (Show,Eq)

-- | Turn a sequence of ints into a Triple
intsToTriple :: S.Seq Word8 -> Triple
intsToTriple [h,t,o] = Triple h t o
intsToTriple xs = error "Can't create triple from xs."

-- | Turn a triple back into an integral value
tripleToInt :: Integral a => Triple -> a
tripleToInt (Triple h t o) = let h' = fromIntegral h
                                 t' = fromIntegral t
                                 o' = fromIntegral o
                              in h' * 100 + t' * 10 + o'

-- | Pull all sequential groups of three numbers from an Integer
-- Start at the rightmost ones place, and go up. Pad the last Triple if
-- necessary.
triples :: Integer -> S.Seq Triple
triples n = intsToTriple <$> S.chunksOf 3 paddedRaw
    where
    raw = rawPlaces n
    lRaw = length raw
    paddingAmt = (3 - lRaw) `mod` 3
    paddedRaw = S.replicate paddingAmt 0 <> raw
    lTriple = ceiling $ fromIntegral lRaw / 3

-- | Get all digits from Integer as a sequence of Ints
rawPlaces :: Integer -> S.Seq Word8
rawPlaces n = S.fromList $ map f $ show n
    where
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9

-- ============================ FULL NUMBER CONVERSION ======================
-- Converts number into a fully pronounceable version, using Conway-Wechsler
-- for zillions and standard English for numbers from 1 - 999999

-- Handles edge cases around a given number before handing it off to
-- convertConway, and then concatenates everything
convert :: [Flag] -> Integer -> T.Text
convert flags = let ?flags = flags in convertWFlags

convertWFlags :: (?flags :: [Flag]) => Integer -> T.Text
convertWFlags n | n == 0    = "zero"
                | otherwise = extract $ getNegative n <>
                              S.intersperse sep (convertConway $ abs n)
    where
    sep = if Newline `elem` ?flags then "\n" else ", "

getNegative :: (?flags :: [Flag]) => Integer -> Fields
getNegative n | n >= 0    = mzero
              | otherwise = pure neg <> pure sepNeg
    where
    sepNeg = if Newline `elem` ?flags then "\n" else " "
    neg    = if KeepNumerals `elem` ?flags then "-" else "negative"

-- Convert a positive number according to Conway-Wechsler system
convertConway :: (?flags :: [Flag]) => Integer -> Fields
convertConway n = join $ S.mapWithIndex f trs
    where
    trs = triples n
    trsLen = length trs
    indexToPower i = toInteger (trsLen - i - 1)
    f i tr = convertRegularWithPower tr $ indexToPower i

-- Convert a triple as it expresses a set of three integers times 10^(N * 3)
convertRegularWithPower :: (?flags :: [Flag]) => Triple -> Integer -> Fields
convertRegularWithPower (Triple 0 0 0) _ = mzero -- Given 0, return empty
convertRegularWithPower tr power = collapse $ numeral <> zillion
    where
    zillion | power == 0 = mzero            -- 10^(3*0) means no zillion prefix
            | power == 1 = pure " thousand" -- 10^(3*1) is a thousand
            | otherwise  = pure " " <> convertPower (power - 1)
                 -- Decrement N by 1 for zillions (N=2 -> million)
    numeral | KeepNumerals `elem` ?flags = pure $ T.pack
                                         $ show $ tripleToInt tr
              -- If KeepNumerals set, just print the number
            | otherwise                 = convertRegular tr

-- ============================ POWER NUMBERS ===============================
-- Converts number as if it were the Nth Conway-Wechsler prefix
-- e.g. 1      -> "million"
--      32     -> "duotrigintillion"
--      100003 -> "millinillitrillion"

-- | Convert an Integer as if it were the Nth zillion, according to the
-- Conway-Wechsler system
convertPower :: (?flags :: [Flag]) => Integer -> Fields
convertPower n = concatWithSep      -- Combine all fields with right separator
               $ (S.|> "on")        -- Append the final "on"
               $ join               -- Join Seq (Seq Text) into Seq Text
               $ convertPowerT      -- Convert each triple to a hun/ten/one prefix
               <$> triples n        -- Extract each triple in the number
    where
    -- joinWithSep :: S.Seq Fields -> Fields
    concatWithSep = if Split `elem` ?flags then S.intersperse " " else id

-- | Convert Triple as expressing a hundreds, tens, and ones place, according
-- to the Conway-Wechsler system
convertPowerT :: Triple -> Fields
convertPowerT (Triple 0 0 0) = pure "nilli" -- Expresses zero for XilliYillion
convertPowerT (Triple 0 0 1) = pure "milli" -- Handle first 9 as Chuquet's names
convertPowerT (Triple 0 0 2) = pure "billi"
convertPowerT (Triple 0 0 3) = pure "trilli"
convertPowerT (Triple 0 0 4) = pure "quadrilli"
convertPowerT (Triple 0 0 5) = pure "quintilli"
convertPowerT (Triple 0 0 6) = pure "sextilli"
convertPowerT (Triple 0 0 7) = pure "septilli"
convertPowerT (Triple 0 0 8) = pure "octilli"
convertPowerT (Triple 0 0 9) = pure "nonilli"
convertPowerT tr@(Triple h t o) = runIfNotNull (mapOnLast appendIlli) x
    where
    x = powerOnes o           -- Convert ones place
        <> pluralizeTriple tr -- Append ones place prefix, if any
        <> powerTens t        -- Convert tens
        <> powerHuns h        -- Convert hundreds
    mapOnLast f sequence = S.adjust' f (S.length sequence - 1) sequence

-- | Append the word "illi" to a text if it is not empty
appendIlli :: T.Text -> T.Text
appendIlli t = fromMaybe T.empty $ do
    (init, last) <- T.unsnoc t
    return $ init `T.append` "illi"

-- | Determine the prefixes for a value in the ones, tens, and hundreds place
-- according to C-W
powerOnes, powerTens, powerHuns :: Word8 -> Fields
powerOnes 0 = mzero
powerOnes 1 = pure "un"
powerOnes 2 = pure "duo"
powerOnes 3 = pure "tre"
powerOnes 4 = pure "quattuor"
powerOnes 5 = pure "quin"
powerOnes 6 = pure "se"
powerOnes 7 = pure "septe"
powerOnes 8 = pure "octo"
powerOnes 9 = pure "nove"

powerTens 0 = mzero
powerTens 1 = pure "deci"
powerTens 2 = pure "viginti"
powerTens 3 = pure "triginta"
powerTens 4 = pure "quadraginta"
powerTens 5 = pure "quinquaginta"
powerTens 6 = pure "sexaginta"
powerTens 7 = pure "septuaginta"
powerTens 8 = pure "octoginta"
powerTens 9 = pure "nonaginta"

powerHuns 0 = mzero
powerHuns 1 = pure "centi"
powerHuns 2 = pure "ducenti"
powerHuns 3 = pure "trecenti"
powerHuns 4 = pure "quadringenti"
powerHuns 5 = pure "quingenti"
powerHuns 6 = pure "sescenti"
powerHuns 7 = pure "septingenti"
powerHuns 8 = pure "octingenti"
powerHuns 9 = pure "nongenti"

-- Inflections
-- | Data type for tracking the inflections a tens/hundreds place can incur on
-- the ones place
data Inf1 = M | N deriving (Show, Eq, Bounded, Enum)
data Inf2 = S | X deriving (Show, Eq, Bounded, Enum)
type Infs = (Maybe Inf1, Maybe Inf2)

-- | Extract inflection of tens or hundreds place and applies it to ones place
-- to obtain correct suffix for ones place
pluralizeTriple :: Triple -> Fields
pluralizeTriple t = inflectOnes (one t) $ tripleInflection t

-- | Given inflection markers and a ones place to apply them to, returns the
-- correct suffix or none at all
inflectOnes :: Word8 -> Infs -> Fields
inflectOnes 3 (_,       Just p)  = pure "s"
inflectOnes 6 (_,       Just p)  = pure $ T.map toLower $ T.pack $ show p
inflectOnes 7 (Just p,  _)       = pure $ T.map toLower $ T.pack $ show p
inflectOnes 9 (Just p,  _)       = pure $ T.map toLower $ T.pack $ show p
inflectOnes _ _                  = mzero

-- | Extract the inflection markers of a Triple, depending on whether a tens or
-- hundred place is immediately adjacent to the ones place
tripleInflection :: Triple -> Infs
tripleInflection (Triple 0 0 _) = (Nothing, Nothing)
tripleInflection (Triple n 0 _) = hunsInflection n
tripleInflection (Triple _ n _) = tensInflection n

-- | Extract the inflection markers of each hundreds place
hunsInflection :: Word8 -> Infs
hunsInflection 1 = (Just N  , Just X)
hunsInflection 2 = (Just N  , Nothing)
hunsInflection 3 = (Just N  , Just S)
hunsInflection 4 = (Just N  , Just S)
hunsInflection 5 = (Just N  , Just S)
hunsInflection 6 = (Just N  , Nothing)
hunsInflection 7 = (Just N  , Nothing)
hunsInflection 8 = (Just M  , Just X)
hunsInflection 9 = (Nothing , Nothing)

-- | Extract the inflection markers of each tens place
tensInflection :: Word8 -> Infs
tensInflection 1 = (Just N  , Nothing)
tensInflection 2 = (Just M  , Just S)
tensInflection 3 = (Just N  , Just S)
tensInflection 4 = (Just N  , Just S)
tensInflection 5 = (Just N  , Just S)
tensInflection 6 = (Just N  , Nothing)
tensInflection 7 = (Just N  , Nothing)
tensInflection 8 = (Just M  , Just X)
tensInflection 9 = (Nothing , Nothing)

-- ============================ REGULAR NUMBERS =============================
-- Converts number as in common English, 
-- e.g. 627 -> "six-hundred and twenty seven"
convertRegular :: Triple -> Fields
convertRegular (Triple 0 0 0) = mzero
convertRegular tr             = wordify $ regularBelow1000 
                              $ tripleToInt tr

regularBelow20 :: Int -> Fields
regularBelow20 n
    | n < 10    = regularOnes n 
    | otherwise = regularTeens n

regularBelow100 :: Int -> Fields
regularBelow100 n 
    | n < 20    = regularBelow20 n
    | otherwise = let (tens, ones) = divMod n 10
                   in if ones == 0
                      then regularTens tens
                      else collapse
                         $ regularTens (n `div` 10) <> pure "-"
                         <> regularOnes (n `mod` 10)

regularBelow1000 :: Int -> Fields
regularBelow1000 n 
    | n < 100   = regularBelow100 n
    | otherwise = let (huns, remainder) = divMod n 100
                      hundreds = regularHuns (n `div` 100)
                      tensones = regularBelow100 (n `mod` 100)
                   in if remainder == 0
                      then hundreds
                      else hundreds <> pure "and" <> tensones

-- | Get the regular name for each multiplicity of a hundred
regularHuns :: Int -> Fields
regularHuns n = runIfNotNull f $ regularOnes n
    where
    f x = collapse $ x <> pure " hundred"

-- | Get the regular name for each multiplicity of one
regularOnes :: Int -> Fields
regularOnes 0 = mempty
regularOnes 1 = pure "one"
regularOnes 2 = pure "two"
regularOnes 3 = pure "three"
regularOnes 4 = pure "four"
regularOnes 5 = pure "five"
regularOnes 6 = pure "six"
regularOnes 7 = pure "seven"
regularOnes 8 = pure "eight"
regularOnes 9 = pure "nine"
regularOnes _ = error "Tried to supply invalid number to regularOnes."

-- | Get the regular name for each number between 10-19 inclusive
regularTeens :: Int -> Fields
regularTeens 10 = pure "ten"
regularTeens 11 = pure "eleven"
regularTeens 12 = pure "twelve"
regularTeens 13 = pure "thirteen"
regularTeens 14 = pure "fourteen"
regularTeens 15 = pure "fifteen"
regularTeens 16 = pure "sixteen"
regularTeens 17 = pure "seventeen"
regularTeens 18 = pure "eighteen"
regularTeens 19 = pure "nineteen"
regularTeens _ = error "Tried to supply invalid number to regularTeens."

-- | Get the regular name for each multiplicity of ten
regularTens :: Int -> Fields
regularTens 0 = mempty
regularTens 1 = pure "ten"
regularTens 2 = pure "twenty"
regularTens 3 = pure "thirty"
regularTens 4 = pure "forty"
regularTens 5 = pure "fifty"
regularTens 6 = pure "sixty"
regularTens 7 = pure "seventy"
regularTens 8 = pure "eighty"
regularTens 9 = pure "ninety"
regularTens _ = error "Tried to supply invalid number to regularTens."


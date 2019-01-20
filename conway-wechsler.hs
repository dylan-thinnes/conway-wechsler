{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Sequence as S
import qualified Data.Text     as T
import Control.Monad (mzero)
import Data.Foldable (toList)

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
                hun :: Int,
                ten :: Int,
                one :: Int
              }
    deriving (Show,Eq)

-- | Turn a sequence of ints into a Triple
intsToTriple :: S.Seq Int -> Triple
intsToTriple s = Triple (S.index s 0) (S.index s 1) (S.index s 2)

-- | Turn a triple back into an integral value
tripleToInt :: Integral a => Triple -> a
tripleToInt (Triple h t o) = fromIntegral $ h * 100 + t * 10 + o

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
rawPlaces :: Integer -> S.Seq Int
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

-- ============================ REGULAR NUMBERS =============================
-- Converts number as in common English, 
-- e.g. 627 -> "six-hundred and twenty seven"

-- | Get the regular name for each multiplicity of a hundred
regularHuns :: Int -> Fields
regularHuns n = runIfNotNull f $ regularOnes n
    where
    f x = collapse $ x <> pure "-hundred"

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
regularTeens 18 = pure "eightteen"
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

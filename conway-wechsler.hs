{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Sequence as S
import qualified Data.Text     as T
import Control.Monad (mzero)
import Data.Foldable (toList)

-- Data type for expressing results of parsing
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

-- Turn a sequence of ints into a Triple
intsToTriple :: S.Seq Int -> Triple
intsToTriple s = Triple (S.index s 0) (S.index s 1) (S.index s 2)

-- Turn a triple back into an integral value
tripleToInt :: Integral a => Triple -> a
tripleToInt (Triple h t o) = fromIntegral $ h * 100 + t * 10 + o

-- Pull all sequential groups of three numbers from an Integer
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

-- Get all digits from Integer as a sequence of Ints
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

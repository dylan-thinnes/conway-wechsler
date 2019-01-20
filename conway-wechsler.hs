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

intsToTriple :: S.Seq Int -> Triple
intsToTriple s = Triple (S.index s 0) (S.index s 1) (S.index s 2)

tripleToInt :: Integral a => Triple -> a
tripleToInt (Triple h t o) = fromIntegral $ h * 100 + t * 10 + o

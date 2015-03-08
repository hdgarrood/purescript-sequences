module Benchmarks where

import qualified Data.Sequence as S
import Data.Foldable
import Data.Array
import Math (floor)

-- Maps numbers from [0,1) to [lo, hi)
integerBetween :: Number -> Number -> Number -> Number
integerBetween lo hi x = floor (((hi - lo) * x) + lo)

insertLotsArray :: [Number] -> [Number]
insertLotsArray = foldr cons []

insertLotsSeq :: [Number] -> S.Seq Number
insertLotsSeq = foldr S.cons S.empty

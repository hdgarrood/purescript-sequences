module Benchmarks where

import qualified Data.Sequence as S
import Data.Foldable
import Data.Traversable
import Data.Array
import Data.Maybe
import Math (floor, sqrt)

-- Maps numbers from [0,1) to [lo, hi)
integerBetween :: Number -> Number -> Number -> Number
integerBetween lo hi x = floor (((hi - lo) * x) + lo)

-- Benchmark 1: insert lots of elements into an empty Array/Seq
insertLotsArray :: [Number] -> [Number]
insertLotsArray = foldr cons []

insertLotsSeq :: [Number] -> S.Seq Number
insertLotsSeq = foldr S.cons S.empty

-- Benchmark 2: traverse an Array/Seq
safeSqrt :: Number -> Maybe Number
safeSqrt x = if x >= 0 then Just (sqrt x) else Nothing

traverseSeq :: S.Seq Number -> Maybe (S.Seq Number)
traverseSeq = traverse safeSqrt

traverseArray :: [Number] -> Maybe [Number]
traverseArray = traverse safeSqrt

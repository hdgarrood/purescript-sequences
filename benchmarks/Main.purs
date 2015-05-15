module Benchmarks where

import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Array
import qualified Data.Sequence as S
import Math (floor, sqrt)
import Test.QuickCheck.Gen
import Test.QuickCheck (Arbitrary, arbitrary)

import Benchmarking

insertLots :: Benchmark (Array Number)
insertLots = Benchmark
  { name: "Insert lots of elements into an empty structure"
  , sizes: (1..50) <#> (*1000)
  , gen: flip vectorOf arbitrary
  , functions: [ { name: "Array", fn: toAny <<< foldr cons []        }
               , { name: "Seq",   fn: toAny <<< foldr S.cons S.empty }
               ]
  }

-- Benchmark 2: traverse an Array/Seq
{-- safeSqrt :: Number -> Maybe Number --}
{-- safeSqrt x = if x >= 0 then Just (sqrt x) else Nothing --}

{-- traverseSeq :: S.Seq Number -> Maybe (S.Seq Number) --}
{-- traverseSeq = traverse safeSqrt --}

{-- traverseArray :: [Number] -> Maybe [Number] --}
{-- traverseArray = traverse safeSqrt --}

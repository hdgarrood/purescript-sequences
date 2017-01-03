module Benchmark.Main where

import Prelude
import Data.Foldable
import Data.Traversable
import Data.Tuple
import Data.Maybe
import Data.Array ((..))
import Data.Array as A
import Data.Sequence as S
import Math (floor, sqrt)
import Test.QuickCheck.Gen (Gen(), vectorOf)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Control.Monad.Eff

import Benchotron.Core
import Benchotron.UI.Console

bimap :: forall a b c d. (a -> b) -> (c -> d) -> Tuple a c -> Tuple b d
bimap f g (Tuple x y) = Tuple (f x) (g y)

benchInsertLots :: Benchmark
benchInsertLots = mkBenchmark
  { slug: "insert-lots"
  , title: "Insert lots of elements into an empty structure"
  , sizes: (1..50) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements to be inserted"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (foldr A.cons [])
               , benchFn "Seq"   (foldr S.cons S.empty)
               ]
  }

benchMap :: Benchmark
benchMap = mkBenchmark
  { slug: "map"
  , title: "Map over a structure"
  , sizes: (1..50) <#> (_ * 1500)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (map f)
               , benchFn' "Seq" (S.fullyForce <<< map f) S.fromFoldable
               ]
  }
  where
  map = (<$>)
  f = (_ * 5)

benchFilter :: Benchmark
benchFilter = mkBenchmark
  { slug: "filter"
  , title: "Filter a structure"
  , sizes: (1..50) <#> (_ * 1500)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (A.filter f)
               , benchFn' "Seq"  (S.filter f) S.fromFoldable
               ]
  }
  where
  f = (_ > 0)

benchApply :: Benchmark
benchApply = mkBenchmark
  { slug: "apply"
  , title: "Apply over a structure with (<*>)"
  , sizes: (1..50) <#> (_ * 100)
  , sizeInterpretation: "Number of elements in the function structure"
  , inputsPerSize: 1
  , gen: \n -> Tuple <$> (map const <$> randomArray n) <*> randomArray 100
  , functions: [ benchFn "Array" (uncurry (<*>))
               , benchFn' "Seq"  (uncurry (<*>)) (bimap S.fromFoldable S.fromFoldable)
               ]
  }

benchConcatMap :: Benchmark
benchConcatMap = mkBenchmark
  { slug: "concat-map"
  , title: "concatMap over a structure"
  , sizes: (1..50) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (A.concatMap f)
               , benchFn' "Seq"  (S.concatMap g) S.fromFoldable
               ]
  }
  where
  f x = [x, x+1, x+2]
  g x = S.cons x (S.cons (x+1) (S.cons (x+2) S.empty))

benchFold :: Benchmark
benchFold = mkBenchmark
  { slug: "fold"
  , title: "Fold a structure"
  , sizes: (1..50) <#> (_ * 1500)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" sum
               , benchFn' "Seq" sum S.fromFoldable
               ]
  }

benchTraverse :: Benchmark
benchTraverse = mkBenchmark
  { slug: "traverse"
  , title: "Traverse a structure"
  , sizes: (1..50) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (traverse Just)
               , benchFn' "Seq" (traverse Just) S.fromFoldable
               ]
  }

benchAppend :: Benchmark
benchAppend = mkBenchmark
  { slug: "append"
  , title: "Append two structures together"
  , sizes: (1..50) <#> (_ * 4000)
  , sizeInterpretation: "Number of elements in each structure"
  , inputsPerSize: 1
  , gen: \n -> Tuple <$> randomArray n <*> randomArray n
  , functions: [ benchFn "Array" (uncurry (<>))
               , benchFn' "Seq"  (S.fullyForce <<< uncurry (<>))
                                 (bimap S.fromFoldable S.fromFoldable)
               ]
  }
  where
  index = flip A.(!!)

benchSort :: Benchmark
benchSort = mkBenchmark
  { slug: "sort"
  , title: "Sort a structure"
  , sizes: (1..8) <#> (_ * 1500)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" A.sort
               , benchFn' "Seq" S.sort S.fromFoldable
               ]
  }

main =
  runSuite
    [ benchInsertLots
    , benchMap
    , benchFilter
    , benchApply
    , benchConcatMap
    , benchFold
    , benchTraverse
    , benchAppend
    , benchSort
    ]

randomArray :: Int -> Gen (Array Int)
randomArray = flip vectorOf arbitrary

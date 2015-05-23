module Benchmark.Main where

import Control.Monad (replicateM)
import Data.Foldable
import Data.Traversable
import Data.Tuple
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Sequence as S
import Math (floor, sqrt)
import Test.QuickCheck.Gen
import Test.QuickCheck (Arbitrary, arbitrary)
import Control.Monad.Eff

import Benchotron.Core
import Benchotron.UI.Console

(..) = A.(..)

bimap :: forall a b c d. (a -> b) -> (c -> d) -> Tuple a c -> Tuple b d
bimap f g (Tuple x y) = Tuple (f x) (g y)

benchInsertLots :: forall e. Benchmark e
benchInsertLots = mkBenchmark
  { slug: "insert-lots"
  , title: "Insert lots of elements into an empty structure"
  , sizes: (1..50) <#> (*1000)
  , sizeInterpretation: "Number of elements to be inserted"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (foldr cons [])
               , benchFn "Seq"   (foldr S.cons S.empty)
               ]
  }

benchMap :: forall e. Benchmark e
benchMap = mkBenchmark
  { slug: "map"
  , title: "Map over a structure"
  , sizes: (1..50) <#> (*1500)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (map f)
               , benchFn' "Seq" (S.fullyForce <<< map f) S.toSeq
               ]
  }
  where
  map = (<$>)
  f = (*5)

benchFilter :: forall e. Benchmark e
benchFilter = mkBenchmark
  { slug: "filter"
  , title: "Filter a structure"
  , sizes: (1..50) <#> (*1500)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (A.filter f)
               , benchFn' "Seq"  (S.filter f) S.toSeq
               ]
  }
  where
  f = (> 0.5)

benchApply :: forall e. Benchmark e
benchApply = mkBenchmark
  { slug: "apply"
  , title: "Apply over a structure with (<*>)"
  , sizes: (1..50) <#> (*100)
  , sizeInterpretation: "Number of elements in the function structure"
  , inputsPerSize: 1
  , gen: \n -> Tuple <$> (A.map const <$> randomArray n) <*> randomArray 100
  , functions: [ benchFn "Array" (uncurry (<*>))
               , benchFn' "Seq"  (uncurry (<*>)) (bimap S.toSeq S.toSeq)
               ]
  }

benchConcatMap :: forall e. Benchmark e
benchConcatMap = mkBenchmark
  { slug: "concat-map"
  , title: "concatMap over a structure"
  , sizes: (1..50) <#> (*1000)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (A.concatMap f)
               , benchFn' "Seq"  (S.concatMap g) S.toSeq
               ]
  }
  where
  f x = [x, x+1, x+2]
  g x = S.cons x (S.cons (x+1) (S.cons (x+2) S.empty))

benchFold :: forall e. Benchmark e
benchFold = mkBenchmark
  { slug: "fold"
  , title: "Fold a structure"
  , sizes: (1..50) <#> (*1500)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" sum
               , benchFn' "Seq" sum S.toSeq
               ]
  }

benchTraverse :: forall e. Benchmark e
benchTraverse = mkBenchmark
  { slug: "traverse"
  , title: "Traverse a structure"
  , sizes: (1..50) <#> (*1000)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (traverseArray Just)
               , benchFn' "Seq" (traverse Just) S.toSeq
               ]
  }

benchAppend :: forall e. Benchmark e
benchAppend = mkBenchmark
  { slug: "append"
  , title: "Append two structures together"
  , sizes: (1..50) <#> (*4000)
  , sizeInterpretation: "Number of elements in each structure"
  , inputsPerSize: 1
  , gen: \n -> Tuple <$> randomArray n <*> randomArray n
  , functions: [ benchFn "Array" (uncurry (<>))
               , benchFn' "Seq"  (S.fullyForce <<< uncurry (<>))
                                 (bimap S.toSeq S.toSeq)
               ]
  }
  where
  index = flip A.(!!)

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
    ]

foreign import randomArray
  """
  function randomArray(n) {
    return function() {
      var arr = []
      for (var i = 0; i < n; i++) {
        arr.push(Math.random())
      }
      return arr;
    }
  } """ :: forall e. Number -> Eff (BenchEffects e) (Array Number)

traverseArray :: forall m a b. (Applicative m) => (a -> m b) -> Array a -> m (Array b)
traverseArray = traverseArrayImpl (<*>) (<$>) pure

-- TODO: Remove after psc-0.7 (when the traverseable array instance is stack-safe).
foreign import traverseArrayImpl
  """
  var traverseArrayImpl = function() {
    function Cont (fn) {
      this.fn = fn;
    }

    function consArray(x) {
      return function (xs) {
        return [x].concat(xs);
      };
    }

    return function (apply) {
      return function (map) {
        return function (pure) {
          return function (f) {
            var buildFrom = function (x, ys) {
              return apply(map(consArray)(f(x)))(ys);
            };

            var go = function (acc, currentLen, xs) {
              if (currentLen === 0) {
                return acc;
              } else {
                var last = xs[currentLen - 1];
                return new Cont(function () {
                  return go(buildFrom(last, acc), currentLen - 1, xs);
                });
              }
            };

            return function (array) {
              var result = go(pure([]), array.length, array);
              while (result instanceof Cont) {
                result = result.fn();
              }

              return result;
            };
          };
        };
      };
    };
  }();
  """ :: forall m a b. (m (a -> b) -> m a -> m b) ->
                       ((a -> b) -> m a -> m b) ->
                       (a -> m a) ->
                       (a -> m b) ->
                       Array a ->
                       m (Array b)

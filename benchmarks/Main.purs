module Benchmark.Main where

import Data.Foldable
import Data.Traversable
import Data.Maybe
import Data.Array
import qualified Data.Sequence as S
import Math (floor, sqrt)
import Test.QuickCheck.Gen
import Test.QuickCheck (Arbitrary, arbitrary)
import Control.Monad.Eff

import Benchotron

benchInsertLots :: forall e. Benchmark e (Array Number)
benchInsertLots =
  { title: "Insert lots of elements into an empty structure"
  , sizes: (1..50) <#> (*1000)
  , sizeInterpretation: "Number of elements to be inserted"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (foldr cons [])
               , benchFn "Seq"   (foldr S.cons S.empty)
               ]
  }

benchTraverse :: forall e. Benchmark e (Array Number)
benchTraverse =
  { title: "Traverse a structure"
  , sizes: (1..50) <#> (*1000)
  , sizeInterpretation: "Number of elements in the structure"
  , inputsPerSize: 1
  , gen: randomArray
  , functions: [ benchFn "Array" (traverseArray Just)
               , benchFn' "Seq" (traverse Just) S.toSeq
               ]
  }

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

-- TODO: Remove when the stack-safe traversable array instance is merged.
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

main = do
  --benchmarkToFile benchInsertLots "tmp/insertLots.json"
  benchmarkToFile benchTraverse "tmp/traverse.json"

-- Benchmark 2: traverse an Array/Seq
{-- safeSqrt :: Number -> Maybe Number --}
{-- safeSqrt x = if x >= 0 then Just (sqrt x) else Nothing --}

{-- traverseSeq :: S.Seq Number -> Maybe (S.Seq Number) --}
{-- traverseSeq = traverse safeSqrt --}

{-- traverseArray :: [Number] -> Maybe [Number] --}
{-- traverseArray = traverse safeSqrt --}

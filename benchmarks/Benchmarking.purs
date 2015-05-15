
module Benchmarking where

import Data.Exists
import Data.Identity
import Data.Tuple
import Data.Array (map, filter)
import Data.Array.Unsafe (head)
import Data.Traversable
import Debug.Trace
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Random
import Test.QuickCheck.Gen

newtype Benchmark a = Benchmark (BenchmarkInner a)

type BenchmarkInner a =
  { name       :: String
  , sizes      :: Array Number
  , gen        :: Number -> Gen a
  , functions  :: Array { name :: String, fn :: a -> Any }
  }

unwrap :: forall a. Benchmark a -> BenchmarkInner a
unwrap (Benchmark x) = x

type Any = Exists Identity

toAny :: forall a. a -> Any
toAny = mkExists <<< Identity

type BenchEffects
  = ( trace :: Trace
    , random :: Random
    )

type BenchmarkM a =
  StateT WrappedState (Eff BenchEffects) a

newtype WrappedState = WrappedState GenState

type Stats =
  { deviation :: Number
  , mean      :: Number
  , moe       :: Number
  , rme       :: Number
  , sample    :: Array Number
  , sem       :: Number
  , variance  :: Number
  }

type Result =
  { size  :: Number
  , stats :: Stats
  }

type ResultSeries =
  { name    :: String
  , results :: Array Result
  }

runBenchmark :: forall a. Benchmark a -> Eff BenchEffects (Array ResultSeries)
runBenchmark bm = do
  seed <- random
  let initialState = WrappedState { newSeed: seed, size: 10 }
  evalStateT (runBenchmark' bm) initialState

inputsPerStep :: Number
inputsPerStep = 100

{-- foreign import runBenchmark'impl --}
{--   """ --}
{--   function runBenchmark$primeimpl(benchmark, rejig, inputsPerStep, initialGenState) { --}
{--     var results = benchmark.sizes.map(function(size) { --}
{--       trace('Benchmarking for n=' + size) --}

{--       var gen = benchmark.gen(size) --}
{--       var inputs = [] --}
{--       var genState = initialGenState --}
{--       for (var i = 0; i < inputsPerStep; i++) { --}
{--         var out = runGen(gen)(genState) --}
{--         inputs.push(out.value) --}
{--         genState = out.state --}
{--       } --}
{--     }) --}

{--     return rejig(results) --}
{--   } --}
{--   """ :: forall a. Fn3 (Benchmark a) --}
{--                        (IntermediateResult -> Array (ResultSeries)) --}
{--                        Number --}
{--                        GenState --}
{--                        (BenchmarkM (Array ResultSeries)) --}


runBenchmark' :: forall a. Benchmark a -> BenchmarkM (Array ResultSeries)
runBenchmark' (Benchmark benchmark) = do
  results <- for benchmark.sizes $ \size -> do
    liftEff $ trace $ "Benchmarking for n=" <> show size

    let gen = benchmark.gen size
    inputs <- state (\(WrappedState s) ->
                let out = runGen (vectorOf inputsPerStep gen) s
                in  Tuple out.value (WrappedState out.state))

    allStats <- for benchmark.functions $ \function -> do
      let f _ = map function.fn inputs
      stats <- StateT $ \s -> runBenchmarkImpl f <#> \x -> Tuple x s
      return { name: function.name, stats: stats }

    return { size: size, allStats: allStats }

  return $ rejig results

type IntermediateResult =
  Array { size :: Number, allStats :: Array { name :: String, stats :: Stats } }

rejig :: IntermediateResult -> Array ResultSeries
rejig [] = []
rejig results = map toSeries names
  where
  r = head results
  names = map _.name r.allStats
  toSeries name =
    { name: name
    , results: map (\o -> { size: o.size
                          , stats: _.stats $ the $ filter ((==) name <<< _.name) o.allStats
                          }) results
    }
  the [x] = x

foreign import runBenchmarkImpl
  """
  function runBenchmarkImpl(fn) {
    var Benchmark = require('benchmark')
    return function() {
      return Benchmark(fn).run().stats
    }
  }
  """ :: forall r. (Unit -> r) -> Eff BenchEffects Stats

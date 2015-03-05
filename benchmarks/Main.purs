module Benchmarks where

import qualified Data.Sequence as S
import Data.Foldable
import Data.Array
import Test.QuickCheck
import Test.QuickCheck.Gen
import Math (floor)

-- this is just a little nicer to work with
integer :: Gen Number
integer = integerBetween 0 100000 <$> arbitrary

-- Maps numbers from [0,1) to [lo, hi)
integerBetween :: Number -> Number -> Number -> Number
integerBetween lo hi x = floor (((hi - lo) * x) + lo)

sizedArrayGen :: Number -> Gen (Array Number)
sizedArrayGen n = vectorOf n integer

r :: forall a. Gen a -> a
r gen = evalGen gen initialState

sizedArray :: Number -> Array Number
sizedArray n = r (sizedArrayGen n)

sizedSeq :: Number -> S.Seq Number
sizedSeq n = S.toSeq (sizedArray n)

initialState :: GenState
initialState = { size: 0, newSeed: 0 }

discard :: forall a. a -> Unit
discard = const unit

insertLotsArray :: Number -> Unit -> Unit
insertLotsArray n =
  let array = sizedArray n
  in  \_ -> discard (foldr cons [] array)

insertLotsSeq :: Number -> Unit -> Unit
insertLotsSeq n =
  let array = sizedArray n
  in  \_ -> discard (foldr S.cons S.empty array)

type Bench = { name :: String, test :: Number -> Unit -> Unit }

b :: String -> (Number -> Unit -> Unit) -> Bench
b = { name: _, test: _ }

benches :: [Bench]
benches =
  [ b "insertLotsArray" insertLotsArray
  , b "insertLotsSeq" insertLotsArray
  ]

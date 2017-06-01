module Tests.Data.Sequence.Ordered (orderedSequenceTests) where

import Prelude (class Eq, class Ord, class Functor, bind, ($), (<=), show, (<>), (==), (>=), (+), (>), Unit, discard)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Foldable (class Foldable, all, foldl, foldr)
import Data.Maybe (Maybe(Just, Nothing))
import Test.QuickCheck ((<?>), (===), quickCheck)
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable)

import Data.Sequence.Ordered (OrdSeq())
import Data.Sequence.Ordered as OrdSeq
import Data.Sequence as S
import Tests.Utils (ArbOSeq(ArbOSeq), ArbSeq(ArbSeq), err, foldableMaximum, foldableMinimum, sortedRev, sorted, foldableSize)

import Type.Proxy (Proxy(Proxy))
import Test.QuickCheck.Laws (A())
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)

arr :: forall a. Ord a => OrdSeq a -> Array a
arr = OrdSeq.toUnfoldable

arrDescending :: forall a. Ord a => OrdSeq a -> Array a
arrDescending = OrdSeq.toUnfoldableDescending

prx :: Proxy (ArbOSeq A)
prx = Proxy

orderedSequenceTests :: forall t.
        Eff
          ( console :: CONSOLE
          , random :: RANDOM
          , exception :: EXCEPTION
          | t
          )
          Unit
orderedSequenceTests = do
  log ""
  log "Data.Sequence.Ordered"
  log "====================="
  log ""

  checkEq prx
  checkSemigroup prx
  checkMonoid prx

  log "Test append"
  quickCheck $ \(ArbOSeq x) (ArbOSeq y) ->
    arr (x <> y) == A.sort (arr x <> arr y :: Array Int)
    <?> ("x: " <> show x <> ", y: " <> show y)

  log "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Int -> Int -> Int) (z :: Int)
    in  foldr f z (OrdSeq.fromFoldable xs) == foldr f z (A.sort xs)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Int -> Int -> Int) (z :: Int)
    in  foldl f z (OrdSeq.fromFoldable xs) == foldl f z (A.sort xs)

  quickCheck $ \xs ->
    A.length xs == foldableSize (OrdSeq.fromFoldable xs :: OrdSeq Int)
  quickCheck $ \(ArbOSeq xs) ->
    A.length (arr xs) == foldableSize (xs :: OrdSeq Int)

  log "Test length/null"
  quickCheck $ \(ArbOSeq xs) ->
    if OrdSeq.empty == (xs :: OrdSeq Int)
      then OrdSeq.null xs
      else OrdSeq.length xs > 0

  quickCheck $ \(ArbOSeq xs) ->
    OrdSeq.length xs + 1 == OrdSeq.length (OrdSeq.insert 0 xs)

  log "Test toUnfoldable is sorted"
  quickCheck $ \(ArbOSeq seq) ->
    sorted (arr seq :: Array Int)

  log "Test toUnfoldableDescending is reverse sorted"
  quickCheck $ \(ArbOSeq seq) ->
    sortedRev (arrDescending seq :: Array Int)

  log "Test least"
  quickCheck $ \(ArbOSeq seq) ->
    let seqLeast :: Maybe Int
        seqLeast = OrdSeq.least seq
        seqMin = foldableMinimum seq
    in seqLeast == seqMin
      <?> err [ "seq = " <> show seq
              , "seqLeast = " <> show seqLeast
              , "seqMin = " <> show seqMin
              ]

  log "Test popLeast"
  quickCheck $ \(ArbOSeq seq) ->
    let seq' = OrdSeq.insert 1 seq -- ensure nonempty
    in case OrdSeq.popLeast seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (_ >= x) seq''

  log "Test greatest"
  quickCheck $ \(ArbOSeq seq) ->
    let actual :: Maybe Int
        actual = OrdSeq.greatest seq
        expected = foldableMaximum (arr seq :: Array Int)
    in actual == expected
      <?> err [ "seq = " <> show seq
              , "expected = " <> show expected
              , "actual = " <> show actual
              ]

  log "Test popGreatest"
  quickCheck $ \(ArbOSeq seq) ->
    let seq' = OrdSeq.insert 0 seq -- ensure nonempty
    in case OrdSeq.popGreatest seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (_ <= x) seq''

  log "Test sort is the same as Data.Array.sort"
  quickCheck $ \array ->
    OrdSeq.sort array === A.sort (array :: Array Int)

  log "Test sort is idempotent"
  quickCheck \(ArbSeq xs) -> sortIdempotent (xs :: S.Seq Int)
  quickCheck \xs          -> sortIdempotent (xs :: Array Int)

  where
  sortIdempotent :: forall f a. Functor f => Unfoldable f => Foldable f => Ord a => Eq (f a) => f a -> Boolean
  sortIdempotent xs = let xs' = OrdSeq.sort xs in xs' == OrdSeq.sort xs'

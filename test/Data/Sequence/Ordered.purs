module Tests.Data.Sequence.Ordered (orderedSequenceTests) where

import Prelude

import           Control.Monad.Eff.Console (log)
import qualified Data.Array                as A
import           Data.Foldable             (Foldable, all, foldl, foldr)
import           Data.Maybe                (Maybe(Just, Nothing))
import           Math                      (floor)
import           Data.Monoid               ()
import           Data.Monoid.Additive      ()
import           Test.QuickCheck           ((<?>), (===), quickCheck)
import           Data.Tuple                (Tuple(Tuple))
import           Data.Unfoldable           (Unfoldable)

import qualified Data.Sequence.Ordered as OrdSeq
import qualified Data.Sequence         as S
import           Tests.Utils
import           TypeClassTests        ()

orderedSequenceTests = do
  log ""
  log "Data.Sequence.Ordered"
  log "====================="
  log ""

  log "Test append"
  quickCheck $ \(ArbOSeq x) (ArbOSeq y) ->
    OrdSeq.fromOrdSeq (x <> y) == A.sort (OrdSeq.fromOrdSeq x <> (OrdSeq.fromOrdSeq y :: Array Int))
    <?> ("x: " <> show x <> ", y: " <> show y)

  log "Test semigroup law: associativity"
  quickCheck $ \(ArbOSeq x) (ArbOSeq y) (ArbOSeq z) ->
    (x <> y) <> z == x <> (y <> z :: OrdSeq.OrdSeq Int)
      <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  log "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Int -> Int -> Int) (z :: Int)
    in  foldr f z (OrdSeq.toOrdSeq xs) == foldr f z (A.sort xs)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Int -> Int -> Int) (z :: Int)
    in  foldl f z (OrdSeq.toOrdSeq xs) == foldl f z (A.sort xs)

  quickCheck $ \xs ->
    A.length xs == foldableSize (OrdSeq.toOrdSeq xs :: OrdSeq.OrdSeq Int)
  quickCheck $ \(ArbOSeq xs) ->
    A.length (OrdSeq.fromOrdSeq xs) == foldableSize (xs :: OrdSeq.OrdSeq Int)

  log "Test length/null"
  quickCheck $ \(ArbOSeq xs) ->
    if OrdSeq.empty == (xs :: OrdSeq.OrdSeq Int)
      then OrdSeq.null xs
      else OrdSeq.length xs > 0

  quickCheck $ \(ArbOSeq xs) ->
    OrdSeq.length xs + 1 == OrdSeq.length (OrdSeq.insert 0 xs)

  log "Test fromOrdSeq is sorted"
  quickCheck $ \(ArbOSeq seq) ->
    sorted (OrdSeq.fromOrdSeq seq :: Array Int)

  log "Test fromOrdSeqDescending is reverse sorted"
  quickCheck $ \(ArbOSeq seq) ->
    sortedRev (OrdSeq.fromOrdSeqDescending seq :: Array Int)

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
         Just (Tuple x seq'') -> all (>= x) seq''

  log "Test greatest"
  quickCheck $ \(ArbOSeq seq) ->
    let actual :: Maybe Int
        actual = OrdSeq.greatest seq
        expected = foldableMaximum (OrdSeq.fromOrdSeq seq :: Array Int)
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
         Just (Tuple x seq'') -> all (<= x) seq''

  log "Test sort is the same as Data.Array.sort"
  quickCheck $ \arr ->
    OrdSeq.sort arr === A.sort (arr :: Array Int)

  log "Test sort is idempotent"
  quickCheck \(ArbSeq xs) -> sortIdempotent (xs :: S.Seq Int)
  quickCheck \xs          -> sortIdempotent (xs :: Array Int)

  where
  sortIdempotent :: forall f a. (Functor f, Unfoldable f, Foldable f, Ord a, Eq (f a)) => f a -> Boolean
  sortIdempotent xs = let xs' = OrdSeq.sort xs in xs' == OrdSeq.sort xs'

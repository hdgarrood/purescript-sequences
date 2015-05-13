
module Tests.Data.Sequence.Ordered where

import qualified Data.Array as A
import Data.Monoid
import Data.Monoid.Additive
import Data.Foldable
import Data.Unfoldable
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Test.QuickCheck
import TypeclassTests
import Math (abs, floor)

import qualified Data.Sequence.Ordered as OrdSeq
import qualified Data.Sequence as S
import Tests.Utils

orderedSequenceTests = do
  trace ""
  trace "Data.Sequence.Ordered"
  trace "====================="
  trace ""

  trace "Test append"
  quickCheck $ \x y ->
    OrdSeq.fromOrdSeq (x <> y) == A.sort (OrdSeq.fromOrdSeq x <> (OrdSeq.fromOrdSeq y :: Array Number))
    <?> ("x: " <> show x <> ", y: " <> show y)

  trace "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: OrdSeq.OrdSeq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  trace "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z (OrdSeq.toOrdSeq xs) == foldr f z (A.sort xs)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z (OrdSeq.toOrdSeq xs) == foldl f z (A.sort xs)

  quickCheck $ \xs -> A.length xs == foldableSize (OrdSeq.toOrdSeq xs :: OrdSeq.OrdSeq Number)
  quickCheck $ \xs -> A.length (OrdSeq.fromOrdSeq xs) == foldableSize (xs :: OrdSeq.OrdSeq Number)

  trace "Test length/null"
  quickCheck $ \xs ->
    if OrdSeq.empty == (xs :: OrdSeq.OrdSeq Number)
      then OrdSeq.null xs
      else OrdSeq.length xs > 0

  quickCheck $ \xs -> isIntegral (OrdSeq.length (xs :: OrdSeq.OrdSeq Number))
  quickCheck $ \xs -> OrdSeq.length xs + 1 == OrdSeq.length (OrdSeq.insert 0 xs)

  trace "Test fromOrdSeq is sorted"
  quickCheck $ \seq ->
    sorted (OrdSeq.fromOrdSeq seq :: Array Number)

  trace "Test fromOrdSeqDescending is reverse sorted"
  quickCheck $ \seq ->
    sortedRev (OrdSeq.fromOrdSeqDescending seq :: Array Number)

  trace "Test least"
  quickCheck $ \seq ->
    OrdSeq.least seq == foldableMinimum (seq :: OrdSeq.OrdSeq Number)
      <?> ("seq: " <> show seq)

  trace "Test popLeast"
  quickCheck $ \seq ->
    let seq' = OrdSeq.insert 1 seq -- ensure nonempty
    in case OrdSeq.popLeast seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (>= x) seq''

  trace "Test greatest"
  quickCheck $ \seq ->
    OrdSeq.greatest seq == foldableMaximum (seq :: OrdSeq.OrdSeq Number)
      <?> ("seq: " <> show seq)

  trace "Test popGreatest"
  quickCheck $ \seq ->
    let seq' = OrdSeq.insert 0 seq -- ensure nonempty
    in case OrdSeq.popGreatest seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (<= x) seq''

  trace "Test sort is the same as Data.Array.sort"
  quickCheck $ \arr ->
    OrdSeq.sort arr === A.sort (arr :: Array Number)

  trace "Test sort is idempotent"
  quickCheck (sortIdempotent :: S.Seq Number -> Boolean)
  quickCheck (sortIdempotent :: Array Number -> Boolean)

  where
  sortIdempotent :: forall f a. (Functor f, Unfoldable f, Foldable f, Ord a, Eq (f a)) => f a -> Boolean
  sortIdempotent xs = let xs' = OrdSeq.sort xs in xs' == OrdSeq.sort xs'


module Tests.Data.Sequence.Ordered where

import qualified Data.Array as A
import Data.Monoid
import Data.Monoid.Additive
import Data.Foldable
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Test.QuickCheck
import TypeclassTests
import Math (abs, floor)

import qualified Data.Sequence.Ordered as OS
import Tests.Utils

orderedSequenceTests = do
  trace ""
  trace "Data.Sequence.Ordered"
  trace "====================="
  trace ""

  trace "Test append"
  quickCheck $ \x y ->
    OS.fromOrdSeq (x <> y) == A.sort (OS.fromOrdSeq x <> (OS.fromOrdSeq y :: Array Number))
    <?> ("x: " <> show x <> ", y: " <> show y)

  trace "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: OS.OrdSeq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  trace "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z (OS.toOrdSeq xs) == foldr f z (A.sort xs)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z (OS.toOrdSeq xs) == foldl f z (A.sort xs)

  quickCheck $ \xs -> A.length xs == foldableSize (OS.toOrdSeq xs :: OS.OrdSeq Number)
  quickCheck $ \xs -> A.length (OS.fromOrdSeq xs) == foldableSize (xs :: OS.OrdSeq Number)

  trace "Test length/null"
  quickCheck $ \xs ->
    if OS.empty == (xs :: OS.OrdSeq Number)
      then OS.null xs
      else OS.length xs > 0

  quickCheck $ \xs -> isIntegral (OS.length (xs :: OS.OrdSeq Number))
  quickCheck $ \xs -> OS.length xs + 1 == OS.length (OS.insert 0 xs)

  trace "Test fromOrdSeq is sorted"
  quickCheck $ \seq ->
    sorted (OS.fromOrdSeq seq :: Array Number)

  trace "Test fromOrdSeqDescending is reverse sorted"
  quickCheck $ \seq ->
    sortedRev (OS.fromOrdSeqDescending seq :: Array Number)

  trace "Test least"
  quickCheck $ \seq ->
    OS.least seq == foldableMinimum (seq :: OS.OrdSeq Number)
      <?> ("seq: " <> show seq)

  trace "Test popLeast"
  quickCheck $ \seq ->
    let seq' = OS.insert 1 seq -- ensure nonempty
    in case OS.popLeast seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (>= x) seq''

  trace "Test greatest"
  quickCheck $ \seq ->
    OS.greatest seq == foldableMaximum (seq :: OS.OrdSeq Number)
      <?> ("seq: " <> show seq)

  trace "Test popGreatest"
  quickCheck $ \seq ->
    let seq' = OS.insert 0 seq -- ensure nonempty
    in case OS.popGreatest seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (<= x) seq''

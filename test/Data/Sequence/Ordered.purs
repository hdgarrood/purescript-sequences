module Tests.Data.Sequence.Ordered (orderedSequenceTests) where

import Prelude

import           Control.Monad.Eff.Console (log)
import qualified Data.Array                as A
import           Data.Foldable             (Foldable, all, foldl, foldr)
import           Data.Maybe                (Maybe(Just, Nothing))
import           Math                      (abs, floor)
import           Data.Monoid               ()
import           Data.Monoid.Additive      ()
import           Test.QuickCheck           ((<?>), (===), quickCheck)
import           Data.Tuple                (Tuple(Tuple))
import           Data.Unfoldable           (Unfoldable)

import qualified Data.Sequence.Ordered as OrdSeq
import qualified Data.Sequence         as S
import           Tests.Utils           ( err
                                       , foldableMaximum
                                       , foldableMinimum
                                       , foldableSize
                                       , sorted
                                       , sortedRev
                                       )
import           TypeClassTests        ()

orderedSequenceTests = do
  log ""
  log "Data.Sequence.Ordered"
  log "====================="
  log ""

  log "Test append"
  quickCheck $ \x y ->
    OrdSeq.fromOrdSeq (x <> y) == A.sort (OrdSeq.fromOrdSeq x <> (OrdSeq.fromOrdSeq y :: Array Number))
    <?> ("x: " <> show x <> ", y: " <> show y)

  log "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: OrdSeq.OrdSeq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  log "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z (OrdSeq.toOrdSeq xs) == foldr f z (A.sort xs)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z (OrdSeq.toOrdSeq xs) == foldl f z (A.sort xs)

  quickCheck $ \xs -> A.length xs == foldableSize (OrdSeq.toOrdSeq xs :: OrdSeq.OrdSeq Number)
  quickCheck $ \xs -> A.length (OrdSeq.fromOrdSeq xs) == foldableSize (xs :: OrdSeq.OrdSeq Number)

  log "Test length/null"
  quickCheck $ \xs ->
    if OrdSeq.empty == (xs :: OrdSeq.OrdSeq Number)
      then OrdSeq.null xs
      else OrdSeq.length xs > 0

  quickCheck $ \xs -> OrdSeq.length xs + 1 == OrdSeq.length (OrdSeq.insert 0.0 xs)

  log "Test fromOrdSeq is sorted"
  quickCheck $ \seq ->
    sorted (OrdSeq.fromOrdSeq seq :: Array Number)

  log "Test fromOrdSeqDescending is reverse sorted"
  quickCheck $ \seq ->
    sortedRev (OrdSeq.fromOrdSeqDescending seq :: Array Number)

  log "Test least"
  quickCheck $ \seq ->
    let seqLeast :: Maybe Number
        seqLeast = OrdSeq.least seq
        seqMin = foldableMinimum seq
    in seqLeast == seqMin
      <?> err [ "seq = " <> show seq
              , "seqLeast = " <> show seqLeast
              , "seqMin = " <> show seqMin
              ]

  log "Test popLeast"
  quickCheck $ \seq ->
    let seq' = OrdSeq.insert 1.0 seq -- ensure nonempty
    in case OrdSeq.popLeast seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (>= x) seq''

  log "Test greatest"
  quickCheck $ \seq ->
    let seqGreatest :: Maybe Number
        seqGreatest = OrdSeq.greatest seq
        seqMax = foldableMaximum seq
    in seqGreatest == seqMax
      <?> err [ "seq = " <> show seq
              , "seqGreatest = " <> show seqGreatest
              , "seqMax = " <> show seqMax
              ]

  log "Test popGreatest"
  quickCheck $ \seq ->
    let seq' = OrdSeq.insert 0 seq -- ensure nonempty
    in case OrdSeq.popGreatest seq' of
         Nothing -> false
         Just (Tuple x seq'') -> all (<= x) seq''

  log "Test sort is the same as Data.Array.sort"
  quickCheck $ \arr ->
    OrdSeq.sort arr === A.sort (arr :: Array Number)

  log "Test sort is idempotent"
  quickCheck (sortIdempotent :: S.Seq Number -> Boolean)
  quickCheck (sortIdempotent :: Array Number -> Boolean)

  where
  sortIdempotent :: forall f a. (Functor f, Unfoldable f, Foldable f, Ord a, Eq (f a)) => f a -> Boolean
  sortIdempotent xs = let xs' = OrdSeq.sort xs in xs' == OrdSeq.sort xs'

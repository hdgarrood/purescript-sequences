
module Tests.Data.Sequence.NonEmpty where

import qualified Data.Array as A
import Data.Monoid
import Data.Monoid.Additive
import Data.Foldable
import Data.Maybe
import Data.Tuple
import Debug.Trace
import Test.QuickCheck
import TypeclassTests

import qualified Data.Sequence as S
import qualified Data.Sequence.NonEmpty as NonEmpty
import Tests.Utils

nonEmptySequenceTests = do
  trace ""
  trace "Data.Sequence.NonEmpty"
  trace "======================"
  trace ""

  trace "Test fromSeq homomorphism"
  quickCheck $ \x y ->
    NonEmpty.fromSeq (x <> y) == NonEmpty.fromSeq x <> (NonEmpty.fromSeq y :: Array Number)
    <?> ("x: " <> show x <> ", y: " <> show y)

  trace "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: NonEmpty.Seq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  let proxy = NonEmpty.singleton 0
  trace "Test functor laws"
  checkFunctor proxy

  trace "Test applicative laws"
  checkApplicative proxy proxy proxy

  trace "Test monad laws"
  checkMonad proxy

  trace "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z xs == foldr f z (NonEmpty.fromSeq xs :: Array Number)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z xs == foldl f z (NonEmpty.fromSeq xs :: Array Number)

  quickCheck $ \xs -> A.length (NonEmpty.fromSeq xs) == foldableSize (xs :: NonEmpty.Seq Number)

  quickCheck $ \xs -> isIntegral (NonEmpty.length (xs :: NonEmpty.Seq Number))
  quickCheck $ \xs -> NonEmpty.length xs + 1 == NonEmpty.length (NonEmpty.cons 0 xs)
  quickCheck $ \xs ->
    let xs' = NonEmpty.cons 0 xs -- ensure xs' has at least one element
    in NonEmpty.length xs' - 1 == S.length (NonEmpty.drop 1 xs')

  trace "Test splitAt/head/last"
  quickCheck $ \idx seq ->
    let idx' :: Number
        idx' = integerBetween 0 (NonEmpty.length seq) idx

        split :: Tuple (S.Seq Number) (S.Seq Number)
        split = NonEmpty.splitAt idx' seq

    in  S.last (fst split) == NonEmpty.index seq (idx' - 1)
          && S.head (snd split) == NonEmpty.index seq idx'
          <?> ("seq: " <> show seq <> ", idx':" <> show idx')

  trace "Test that adjust is safe"
  quickCheck $ \seq ->
    let f n = NonEmpty.adjust id n (seq :: NonEmpty.Seq Number)
    in f (-1) == f (NonEmpty.length seq)

  trace "Test that index is safe"
  quickCheck $ \seq ->
    let f n = NonEmpty.index (seq :: NonEmpty.Seq Number) n
    in f (-1) == Nothing && f (NonEmpty.length seq) == Nothing

  trace "Test inBounds"
  quickCheck $ \seq ->
    let seq' = NonEmpty.cons 0 seq
        lowerBound = 0
        upperBound = NonEmpty.length seq' - 1
    in NonEmpty.inBounds seq' lowerBound && NonEmpty.inBounds seq' upperBound
        && not (NonEmpty.inBounds seq' (lowerBound - 1))
        && not (NonEmpty.inBounds seq' (upperBound + 1))

  trace "Test adjust"
  quickCheck $ \seq idx ->
    let seq' = const 0 <$> NonEmpty.cons 0 seq
        idx' = integerBetween 0 (NonEmpty.length seq') idx
        result = sum (NonEmpty.adjust (+1) idx' seq')
    in  result == 1 <?> "seq': " <> show seq' <> ", result: " <> show result

  trace "Test take"
  quickCheck $ \seq n ->
    let result = S.length (NonEmpty.take n (seq :: NonEmpty.Seq Number))
    in 0 <= result && result <= n

  trace "Test drop"
  quickCheck $ \seq n ->
    let dropped = S.length (NonEmpty.drop n seq) - NonEmpty.length (seq :: NonEmpty.Seq Number)
    in 0 <= dropped && dropped <= n

  trace "Test filter"
  quickCheck $ \seq -> S.null (NonEmpty.filter (const false) (seq :: NonEmpty.Seq Number))
  quickCheck $ \seq -> NonEmpty.filter (const true) seq === (NonEmpty.toPlain seq :: S.Seq Number)
  quickCheck $ \seq f -> all f (NonEmpty.filter f (seq :: NonEmpty.Seq Number))

  trace "Test length"
  quickCheck $ \seq -> NonEmpty.length (seq :: NonEmpty.Seq Number) >= 1 <?> show seq

  trace "Test cons/uncons"
  quickCheck $ \seq x ->
    NonEmpty.uncons (NonEmpty.cons x seq) === Tuple (x :: Number) (NonEmpty.toPlain seq)
  quickCheck $ \seq x ->
    NonEmpty.unsnoc (NonEmpty.snoc seq x) === Tuple (NonEmpty.toPlain seq) (x :: Number)

  trace "Test init"
  quickCheck $ \seq ->
    NonEmpty.init seq === NonEmpty.take (NonEmpty.length seq - 1) (seq :: NonEmpty.Seq Number)

  trace "Test tail"
  quickCheck $ \seq ->
    NonEmpty.tail seq === NonEmpty.drop 1 (seq :: NonEmpty.Seq Number)

  trace "Test head"
  quickCheck $ \seq x ->
    NonEmpty.head (NonEmpty.cons x seq) === (x :: Number)

  trace "Test last"
  quickCheck $ \seq x ->
    NonEmpty.last (NonEmpty.snoc seq x) === (x :: Number)

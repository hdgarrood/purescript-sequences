
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
import Math (abs, floor)

import qualified Data.Sequence as S
import qualified Data.Sequence.NonEmpty as NES
import Tests.Data.Sequence ()

instance arbSeq :: (Arbitrary a) => Arbitrary (NES.Seq a) where
  arbitrary = NES.Seq <$> arbitrary <*> arbitrary

foldableSize :: forall f a. (Foldable f) => f a -> Number
foldableSize = runAdditive <<< foldMap (const (Additive 1))

check1 :: forall p. (Testable p) => p -> QC Unit
check1 = quickCheck' 1

integerBetween :: Number -> Number -> Number -> Number
integerBetween lo hi x = (floor x % hi - lo) + lo

isIntegral :: Number -> Boolean
isIntegral x = complement (complement x) == x

nonEmptySequenceTests = do
  trace ""
  trace "Data.Sequence.NonEmpty"
  trace "======================"
  trace ""

  trace "Test fromSeq homomorphism"
  quickCheck $ \x y ->
    NES.fromSeq (x <> y) == NES.fromSeq x <> (NES.fromSeq y :: Array Number)
    <?> ("x: " <> show x <> ", y: " <> show y)

  trace "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: NES.Seq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  let proxy = NES.singleton 0
  trace "Test functor laws"
  checkFunctor proxy

  trace "Test applicative laws"
  checkApplicative proxy proxy proxy

  trace "Test monad laws"
  checkMonad proxy

  trace "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z xs == foldr f z (NES.fromSeq xs :: Array Number)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z xs == foldl f z (NES.fromSeq xs :: Array Number)

  quickCheck $ \xs -> A.length (NES.fromSeq xs) == foldableSize (xs :: NES.Seq Number)

  quickCheck $ \xs -> isIntegral (NES.length (xs :: NES.Seq Number))
  quickCheck $ \xs -> NES.length xs + 1 == NES.length (NES.cons 0 xs)
  quickCheck $ \xs ->
    let xs' = NES.cons 0 xs -- ensure xs' has at least one element
    in NES.length xs' - 1 == S.length (NES.drop 1 xs')

  trace "Test splitAt/head/last"
  quickCheck $ \idx seq ->
    let idx' :: Number
        idx' = integerBetween 0 (NES.length seq) idx

        split :: Tuple (S.Seq Number) (S.Seq Number)
        split = NES.splitAt idx' seq

    in  S.last (fst split) == NES.index seq (idx' - 1)
          && S.head (snd split) == NES.index seq idx'
          <?> ("seq: " <> show seq <> ", idx':" <> show idx')

  trace "Test that adjust is safe"
  quickCheck $ \seq ->
    let f n = NES.adjust id n (seq :: NES.Seq Number)
    in f (-1) == f (NES.length seq)

  trace "Test that index is safe"
  quickCheck $ \seq ->
    let f n = NES.index (seq :: NES.Seq Number) n
    in f (-1) == Nothing && f (NES.length seq) == Nothing

  trace "Test inBounds"
  quickCheck $ \seq ->
    let seq' = NES.cons 0 seq
        lowerBound = 0
        upperBound = NES.length seq' - 1
    in NES.inBounds seq' lowerBound && NES.inBounds seq' upperBound
        && not (NES.inBounds seq' (lowerBound - 1))
        && not (NES.inBounds seq' (upperBound + 1))

  trace "Test adjust"
  quickCheck $ \seq idx ->
    let seq' = const 0 <$> NES.cons 0 seq
        idx' = integerBetween 0 (NES.length seq') idx
        result = sum (NES.adjust (+1) idx' seq')
    in  result == 1 <?> "seq': " <> show seq' <> ", result: " <> show result

  trace "Test take"
  quickCheck $ \seq n ->
    let result = S.length (NES.take n (seq :: NES.Seq Number))
    in 0 <= result && result <= n

  trace "Test drop"
  quickCheck $ \seq n ->
    let dropped = S.length (NES.drop n seq) - NES.length (seq :: NES.Seq Number)
    in 0 <= dropped && dropped <= n

  trace "Test filter"
  quickCheck $ \seq -> S.null (NES.filter (const false) (seq :: NES.Seq Number))
  quickCheck $ \seq -> NES.filter (const true) seq === (NES.toPlain seq :: S.Seq Number)
  quickCheck $ \seq f -> all f (NES.filter f (seq :: NES.Seq Number))

  trace "Test length"
  quickCheck $ \seq -> NES.length (seq :: NES.Seq Number) >= 1 <?> show seq

  trace "Test cons/uncons"
  quickCheck $ \seq x ->
    NES.uncons (NES.cons x seq) === Tuple (x :: Number) (NES.toPlain seq)
  quickCheck $ \seq x ->
    NES.unsnoc (NES.snoc seq x) === Tuple (NES.toPlain seq) (x :: Number)

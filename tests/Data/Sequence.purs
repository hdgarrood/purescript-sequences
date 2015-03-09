module Tests.Data.Sequence where

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
import qualified Data.FingerTree as FT

instance arbSeq :: (Arbitrary a) => Arbitrary (S.Seq a) where
  arbitrary = (S.toSeq :: Array a -> S.Seq a) <$> arbitrary

foldableSize :: forall f a. (Foldable f) => f a -> Number
foldableSize = runAdditive <<< foldMap (const (Additive 1))

check1 :: forall p. (Testable p) => p -> QC Unit
check1 = quickCheck' 1

integerBetween :: Number -> Number -> Number -> Number
integerBetween lo hi x = (floor x % hi - lo) + lo

isIntegral :: Number -> Boolean
isIntegral x = complement (complement x) == x

sequenceTests = do
  trace "Test append"
  quickCheck $ \x y ->
    S.fromSeq (x <> y) == S.fromSeq x <> (S.fromSeq y :: Array Number)
    <?> ("x: " <> show x <> ", y: " <> show y)

  trace "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: S.Seq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  trace "Test monoid law: left identity"
  quickCheck $ \x -> (mempty <> x) == (x :: S.Seq Number)
    <?> ("x: " <> show x)

  trace "Test monoid law: right identity"
  quickCheck $ \x -> (x <> mempty) == (x :: S.Seq Number)
    <?> ("x: " <> show x)

  let proxy = S.singleton 0
  trace "Test functor laws"
  checkFunctor proxy

  trace "Test applicative laws"
  checkApplicative proxy proxy proxy

  trace "Test monad laws"
  checkMonad proxy

  trace "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z (S.toSeq xs) == foldr f z (xs :: Array Number)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z (S.toSeq xs) == foldl f z (xs :: Array Number)

  quickCheck $ \xs -> A.length xs == foldableSize (S.toSeq xs :: S.Seq Number)
  quickCheck $ \xs -> A.length (S.fromSeq xs) == foldableSize (xs :: S.Seq Number)

  trace "Test length/null"
  quickCheck $ \xs ->
    if S.empty == (xs :: S.Seq Number) then S.null xs else S.length xs > 0

  quickCheck $ \xs -> isIntegral (S.length (xs :: S.Seq Number))
  quickCheck $ \xs -> S.length xs + 1 == S.length (S.cons 0 xs)
  quickCheck $ \xs ->
    let xs' = S.cons 0 xs -- ensure xs' has at least one element
    in S.length xs' - 1 == S.length (S.drop 1 xs')

  trace "Test splitAt/head/last"
  quickCheck $ \idx seq ->
    let idx' :: Number
        idx' = integerBetween 0 (S.length seq) idx

        split :: Tuple (S.Seq Number) (S.Seq Number)
        split = S.splitAt idx' seq

    in  S.last (fst split) == S.index seq (idx' - 1)
          && S.head (snd split) == S.index seq idx'
          <?> ("seq: " <> show seq <> ", idx':" <> show idx')

  trace "Test show instance"
  check1 $ \xs -> show (S.toSeq xs) == ("toSeq " <> show (xs :: Array Number))

  trace "Test that adjust is safe"
  quickCheck $ \seq ->
    let f n = S.adjust id n (seq :: S.Seq Number)
    in f (-1) == f (S.length seq)

  trace "Test that index is safe"
  quickCheck $ \seq ->
    let f n = S.index (seq :: S.Seq Number) n
    in f (-1) == Nothing && f (S.length seq) == Nothing

  trace "Test inBounds"
  quickCheck $ \seq ->
    let seq' = S.cons 0 seq
        lowerBound = 0
        upperBound = S.length seq' - 1
    in S.inBounds seq' lowerBound && S.inBounds seq' upperBound
        && not (S.inBounds seq' (lowerBound - 1))
        && not (S.inBounds seq' (upperBound + 1))

  trace "Test adjust"
  quickCheck $ \seq idx ->
    let seq' = const 0 <$> S.cons 0 seq
        idx' = integerBetween 0 (S.length seq') idx
        result = sum (S.adjust (+1) idx' seq')
    in  result == 1 <?> "seq': " <> show seq' <> ", result: " <> show result

  trace "Test take"
  quickCheck $ \seq n ->
    let result = S.length (S.take n (seq :: S.Seq Number))
    in 0 <= result && result <= n

  trace "Test drop"
  quickCheck $ \seq n ->
    let dropped = S.length (S.drop n seq) - S.length (seq :: S.Seq Number)
    in 0 <= dropped && dropped <= n

  trace "Test filter"
  quickCheck $ \seq -> S.null (S.filter (const false) (seq :: S.Seq Number))
  quickCheck $ \seq -> S.filter (const true) seq === (seq :: S.Seq Number)
  quickCheck $ \seq f -> all f (S.filter f (seq :: S.Seq Number))

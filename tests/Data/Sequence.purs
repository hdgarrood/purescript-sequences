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

import qualified Data.Sequence as S
import qualified Data.FingerTree as FT

instance arbSeq :: (Arbitrary a) => Arbitrary (S.Seq a) where
  arbitrary = (S.toSeq :: [a] -> S.Seq a) <$> arbitrary

foldableSize :: forall f a. (Foldable f) => f a -> Number
foldableSize = runAdditive <<< foldMap (const (Additive 1))

sequenceTests = do
  trace "Test append"
  quickCheck $ \x y ->
    S.fromSeq (x <> y) == S.fromSeq x <> (S.fromSeq y :: [Number])
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
    in  foldr f z (S.toSeq xs) == foldr f z (xs :: [Number])

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z (S.toSeq xs) == foldl f z (xs :: [Number])

  quickCheck $ \xs -> A.length xs == foldableSize (S.toSeq xs :: S.Seq Number)
  quickCheck $ \xs -> A.length (S.fromSeq xs) == foldableSize (xs :: S.Seq Number)

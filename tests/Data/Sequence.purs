module Tests.Data.Sequence where

import Data.Monoid
import Data.Maybe
import Debug.Trace
import Test.QuickCheck
import TypeclassTests

import qualified Data.Sequence as S
import qualified Data.FingerTree as FT

instance arbSeq :: (Arbitrary a) => Arbitrary (S.Seq a) where
  arbitrary = (S.toSeq :: [a] -> S.Seq a) <$> arbitrary

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

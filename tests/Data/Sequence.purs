module Tests.Data.Sequence where

import Data.Maybe
import Debug.Trace
import Test.QuickCheck

import qualified Data.Sequence as S
import qualified Data.FingerTree as FT

instance arbSeq :: (Arbitrary a) => Arbitrary (S.Seq a) where
  arbitrary = S.fromArray <$> arbitrary

sequenceTests = do
  trace "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: S.Seq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

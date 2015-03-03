module Tests.Data.Sequence where

import Data.Maybe
import Test.QuickCheck

import qualified Data.Sequence as S
import qualified Data.FingerTree as FT

instance arbSeq :: Arbitrary (FT.FingerTree S.Size (S.Elem a)) where
  arbitrary = S.fromArray <$> arbitrary

sequenceTests = do
  trace "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: S.Seq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

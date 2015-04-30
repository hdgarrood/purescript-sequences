
module Tests.Utils where

import Math (abs, floor)
import qualified Data.Array as A
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Monoid.Additive
import Test.QuickCheck

import qualified Data.Sequence as S
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Sequence.Ordered as OS

instance arbSeq :: (Arbitrary a) => Arbitrary (S.Seq a) where
  arbitrary = (S.toSeq :: Array a -> S.Seq a) <$> arbitrary

instance arbNonEmptySeq :: (Arbitrary a) => Arbitrary (NES.Seq a) where
  arbitrary = NES.Seq <$> arbitrary <*> arbitrary

instance arbOrdSeq :: (Ord a, Arbitrary a) => Arbitrary (OS.OrdSeq a) where
  arbitrary = (OS.toOrdSeq :: Array a -> OS.OrdSeq a) <$> arbitrary

foldableSize :: forall f a. (Foldable f) => f a -> Number
foldableSize = runAdditive <<< foldMap (const (Additive 1))

check1 :: forall p. (Testable p) => p -> QC Unit
check1 = quickCheck' 1

integerBetween :: Number -> Number -> Number -> Number
integerBetween lo hi x = (floor x % hi - lo) + lo

isIntegral :: Number -> Boolean
isIntegral x = complement (complement x) == x

sorted :: forall a. (Show a, Ord a) => Array a -> _
sorted xs = xs == A.sort xs
              <?> show xs <> " is not sorted."

sortedRev :: forall a. (Show a, Ord a) => Array a -> _
sortedRev xs = xs == A.reverse (A.sort xs)
                <?> show xs <> " is not sorted in reverse order."

newtype Min a = Min a

runMin :: forall a. Min a -> a
runMin (Min a) = a

instance eqMin :: (Eq a) => Eq (Min a) where
  (==) (Min a) (Min b) = a == b
  (/=) a b = not (a == b)

instance ordMin :: (Ord a) => Ord (Min a) where
  compare (Min a) (Min b) = compare a b

instance semigroupMin :: (Ord a) => Semigroup (Min a) where
  (<>) a b =
    case compare a b of
      LT -> a
      EQ -> a
      GT -> b

foldableMinimum :: forall f a. (Ord a, Foldable f) => f a -> Maybe a
foldableMinimum = (<$>) runMin <<< foldMap (Just <<< Min)

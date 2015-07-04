module Tests.Utils where

import Prelude

import qualified Data.Array as A
import Data.Foldable
import Data.Int (fromNumber, toNumber)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid
import Data.Monoid.Additive
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import qualified Data.Sequence as S
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Sequence.Ordered as OS

instance arbSeq :: (Arbitrary a) => Arbitrary (S.Seq a) where
  arbitrary = (S.toSeq :: Array a -> S.Seq a) <$> arbitrary

instance arbNonEmptySeq :: (Arbitrary a) => Arbitrary (NES.Seq a) where
  arbitrary = NES.Seq <$> arbitrary <*> arbitrary

instance arbOrdSeq :: (Ord a, Arbitrary a) => Arbitrary (OS.OrdSeq a) where
  arbitrary = (OS.toOrdSeq :: Array a -> OS.OrdSeq a) <$> arbitrary

foldableSize :: forall f a. (Foldable f) => f a -> Int
foldableSize = runAdditive <<< foldMap (const (Additive 1))

check1 :: forall p. (Testable p) => p -> QC Unit
check1 = quickCheck' 1

abs :: Int -> Int
abs x = if x < 0 then (-x) else x

integerBetween :: Int -> Int -> Int -> Int
integerBetween lo hi x = (abs x `mod` hi - lo) + lo

sorted :: forall a. (Show a, Ord a) => Array a -> _
sorted xs = xs == A.sort xs
              <?> show xs <> " is not sorted."

sortedRev :: forall a. (Show a, Ord a) => Array a -> _
sortedRev xs = xs == A.reverse (A.sort xs)
                <?> show xs <> " is not sorted in reverse order."

-------------
-- Min/Max
-- TODO: Remove this (once 0.7.0 is released)

newtype Min a = Min a

runMin :: forall a. Min a -> a
runMin (Min a) = a

instance eqMin :: (Eq a) => Eq (Min a) where
  eq (Min a) (Min b) = a == b

instance ordMin :: (Ord a) => Ord (Min a) where
  compare (Min a) (Min b) = compare a b

instance semigroupMin :: (Ord a) => Semigroup (Min a) where
  append a b =
    case compare a b of
      LT -> a
      EQ -> a
      GT -> b

-- Non-standard appending of `Semigroup a => Maybe a` where `Just x` takes
-- takes priority over `Nothing`. See
-- https://www.reddit.com/r/haskell/comments/39tumu/make_semigroup_a_superclass_of_monoid/cs6hlca
-- (referenced by Phil Freeman in https://github.com/purescript/purescript-maybe/pull/11)
-- for the reasoning why the PureScript standard differs from that of Haskell
maybeAppend' :: forall a. (Semigroup a) => Maybe a -> Maybe a -> Maybe a
maybeAppend' Nothing y = y
maybeAppend' x Nothing = x
maybeAppend' x y       = append <$> x <*> y

foldableMinimum :: forall f a. (Ord a, Foldable f) => f a -> Maybe a
foldableMinimum = map runMin <<< foldr (maybeAppend' <<< Just <<< Min) Nothing

newtype Max a = Max a

runMax :: forall a. Max a -> a
runMax (Max a) = a

instance eqMax :: (Eq a) => Eq (Max a) where
  eq (Max a) (Max b) = a == b

instance ordMax :: (Ord a) => Ord (Max a) where
  compare (Max a) (Max b) = compare a b

instance semigroupMax :: (Ord a) => Semigroup (Max a) where
  append a b =
    case compare a b of
      LT -> b
      EQ -> a
      GT -> a

foldableMaximum :: forall f a. (Ord a, Foldable f) => f a -> Maybe a
foldableMaximum = map runMax <<< foldr (maybeAppend' <<< Just <<< Max) Nothing

-------------------------------------------------------------------------------

err :: Array String -> String
err messages = "Did not hold for: " <> intercalate ", " messages

module Tests.Utils where

import Prelude

import qualified Data.Array as A
import Data.Function (on)
import Data.Foldable
import Data.Int (fromNumber, toNumber)
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Monoid
import Data.Monoid.Additive
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen())

import qualified Data.Sequence as S
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Sequence.Ordered as OS

-----------------------------
--- newtype wrappers

-----------------
-- Data.Sequence

newtype ArbSeq a = ArbSeq (S.Seq a)
unArbSeq (ArbSeq xs) = xs

instance eqArbSeq :: (Eq a) => Eq (ArbSeq a) where
  eq = eq `on` unArbSeq

instance functorArbSeq :: Functor ArbSeq where
  map f = ArbSeq <<< map f <<< unArbSeq

instance applyArbSeq :: Apply ArbSeq where
  apply fs xs = ArbSeq (apply (unArbSeq fs) (unArbSeq xs))

instance applicativeArbSeq :: Applicative ArbSeq where
  pure = ArbSeq <<< pure

instance bindArbSeq :: Bind ArbSeq where
  bind xs f = ArbSeq (bind (unArbSeq xs) (unArbSeq <<< f))

instance monadArbSeq :: Monad ArbSeq

instance showArbSeq :: (Show a) => Show (ArbSeq a) where
  show = show <<< unArbSeq

instance arbitraryArbSeq :: (Arbitrary a) => Arbitrary (ArbSeq a) where
  arbitrary = (ArbSeq <<< S.fromFoldable) <$> (arbitrary :: Gen (Array a))

--------------------------
-- Data.Sequence.NonEmpty

newtype ArbNESeq a = ArbNESeq (NES.Seq a)
unArbNESeq (ArbNESeq xs) = xs

instance eqArbNESeq :: (Eq a) => Eq (ArbNESeq a) where
  eq = eq `on` unArbNESeq

instance functorArbNESeq :: Functor ArbNESeq where
  map f = ArbNESeq <<< map f <<< unArbNESeq

instance applyArbNESeq :: Apply ArbNESeq where
  apply fs xs = ArbNESeq (apply (unArbNESeq fs) (unArbNESeq xs))

instance applicativeArbNESeq :: Applicative ArbNESeq where
  pure = ArbNESeq <<< pure

instance bindArbNESeq :: Bind ArbNESeq where
  bind xs f = ArbNESeq (bind (unArbNESeq xs) (unArbNESeq <<< f))

instance monadArbNESeq :: Monad ArbNESeq

instance showArbNESeq :: (Show a) => Show (ArbNESeq a) where
  show = show <<< unArbNESeq

instance arbitraryArbNESeq :: (Arbitrary a) => Arbitrary (ArbNESeq a) where
  arbitrary = ArbNESeq <$> (NES.Seq <$> arbitrary <*> (unArbSeq <$> arbitrary))

--------------------------
-- Data.Sequence.Ordered

newtype ArbOSeq a = ArbOSeq (OS.OrdSeq a)
unArbOSeq (ArbOSeq xs) = xs

instance eqArbOSeq :: (Eq a) => Eq (ArbOSeq a) where
  eq = eq `on` unArbOSeq

instance showArbOSeq :: (Show a) => Show (ArbOSeq a) where
  show = show <<< unArbOSeq

instance arbitraryArbOrdSeq :: (Ord a, Arbitrary a) => Arbitrary (ArbOSeq a) where
  arbitrary = (ArbOSeq <<< OS.fromFoldable) <$> (arbitrary :: Gen (Array a))

--------------------------

foldableSize :: forall f a. (Foldable f) => f a -> Int
foldableSize = runAdditive <<< foldMap (const (Additive 1))

check1 :: forall e p. (Testable p) => p -> QC e Unit
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

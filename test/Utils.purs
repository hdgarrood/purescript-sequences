module Tests.Utils where

import Prelude

import Data.Array as A
import Data.Function (on)
import Data.Foldable (class Foldable, intercalate, foldr, foldMap)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(Additive))
import Data.Newtype (un)
import Control.Alt (class Alt, (<|>))
import Control.Plus (class Plus, empty)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Test.QuickCheck (class Testable, QC, (<?>), quickCheck', Result)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen())

import Data.Sequence as S
import Data.Sequence.NonEmpty as NES
import Data.Sequence.Ordered as OS

-----------------------------
--- newtype wrappers

-----------------
-- Data.Sequence

newtype ArbSeq a = ArbSeq (S.Seq a)
unArbSeq :: forall b. ArbSeq b -> S.Seq b
unArbSeq (ArbSeq xs) = xs

instance eqArbSeq :: (Eq a) => Eq (ArbSeq a) where
  eq = eq `on` unArbSeq

instance ordArbSeq :: (Ord a) => Ord (ArbSeq a) where
  compare = compare `on` unArbSeq

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

instance semigroupArbSeq :: Semigroup (ArbSeq a) where
  append (ArbSeq xs) (ArbSeq ys) = ArbSeq (xs <> ys)

instance monoidArbSeq :: Monoid (ArbSeq a) where
  mempty = ArbSeq mempty

instance altArbSeq :: Alt ArbSeq where
  alt (ArbSeq xs) (ArbSeq ys) = ArbSeq (xs <|> ys)

instance plusArbSeq :: Plus ArbSeq where
  empty = ArbSeq empty

instance alternativeArbseq :: Alternative ArbSeq

instance monadPlusArbSeq :: MonadPlus ArbSeq

instance monadZeroArbSeq :: MonadZero ArbSeq

instance arbitraryArbSeq :: (Arbitrary a) => Arbitrary (ArbSeq a) where
  arbitrary = (ArbSeq <<< S.fromFoldable) <$> (arbitrary :: Gen (Array a))

--------------------------
-- Data.Sequence.NonEmpty

newtype ArbNESeq a = ArbNESeq (NES.Seq a)
unArbNESeq :: forall b. ArbNESeq b -> NES.Seq b
unArbNESeq (ArbNESeq xs) = xs

instance eqArbNESeq :: (Eq a) => Eq (ArbNESeq a) where
  eq = eq `on` unArbNESeq

instance ordArbNESeq :: (Ord a) => Ord (ArbNESeq a) where
  compare = compare `on` unArbNESeq

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

instance semigroupArbNESeq :: Semigroup (ArbNESeq a) where
  append (ArbNESeq xs) (ArbNESeq ys) = ArbNESeq (xs <> ys)

instance altArbNESeq :: Alt ArbNESeq where
  alt (ArbNESeq xs) (ArbNESeq ys) = ArbNESeq (xs <|> ys)

instance arbitraryArbNESeq :: (Arbitrary a) => Arbitrary (ArbNESeq a) where
  arbitrary = ArbNESeq <$> (NES.Seq <$> arbitrary <*> (unArbSeq <$> arbitrary))

--------------------------
-- Data.Sequence.Ordered

newtype ArbOSeq a = ArbOSeq (OS.OrdSeq a)
unArbOSeq :: forall b. ArbOSeq b -> OS.OrdSeq b
unArbOSeq (ArbOSeq xs) = xs

instance eqArbOSeq :: (Eq a) => Eq (ArbOSeq a) where
  eq = eq `on` unArbOSeq

instance showArbOSeq :: (Show a) => Show (ArbOSeq a) where
  show = show <<< unArbOSeq

instance semigroupArbOrdSeq :: (Ord a) => Semigroup (ArbOSeq a) where
  append (ArbOSeq xs) (ArbOSeq ys) = ArbOSeq (xs <> ys)

instance monoidArbOrdSeq :: (Ord a) => Monoid (ArbOSeq a) where
  mempty = ArbOSeq mempty

instance arbitraryArbOrdSeq :: (Ord a, Arbitrary a) => Arbitrary (ArbOSeq a) where
  arbitrary = (ArbOSeq <<< OS.fromFoldable) <$> (arbitrary :: Gen (Array a))

--------------------------

foldableSize :: forall f a. Foldable f => f a -> Int
foldableSize = un Additive <<< foldMap (const (Additive 1))

check1 :: forall e p. (Testable p) => p -> QC e Unit
check1 = quickCheck' 1

abs :: Int -> Int
abs x = if x < 0 then (-x) else x

integerBetween :: Int -> Int -> Int -> Int
integerBetween lo hi x = (abs x `mod` hi - lo) + lo

sorted :: forall a. Show a => Ord a => Array a -> Result
sorted xs = xs == A.sort xs
              <?> show xs <> " is not sorted."

sortedRev :: forall a. Show a => Ord a => Array a -> Result
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
maybeAppend' :: forall a. Semigroup a => Maybe a -> Maybe a -> Maybe a
maybeAppend' Nothing y = y
maybeAppend' x Nothing = x
maybeAppend' x y       = append <$> x <*> y

foldableMinimum :: forall f a. Ord a => Foldable f => f a -> Maybe a
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

foldableMaximum :: forall f a. Ord a => Foldable f => f a -> Maybe a
foldableMaximum = map runMax <<< foldr (maybeAppend' <<< Just <<< Max) Nothing

-------------------------------------------------------------------------------

err :: Array String -> String
err messages = "Did not hold for: " <> intercalate ", " messages

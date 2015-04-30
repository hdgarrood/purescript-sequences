
-- | This module defines a sequence where elements are always kept in ascending
-- | order. This enables constant time access to the least and greatest
-- | elements, in addition to logarithmic time partitioning.
-- |
-- | The module is intended to be imported qualified, to avoid ambiguity or
-- | name clashes. For example, `import Data.Sequence.Ordered as OS`.

module Data.Sequence.Ordered where

import Data.Lazy
import Data.Tuple
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Data.Sequence.Internal
import qualified Data.FingerTree as FT

data Key a = NoKey | Key a

instance eqKey :: (Eq a) => Eq (Key a) where
  (==) (Key a) (Key b) = a == b
  (==) NoKey NoKey = true
  (==) _ _ = false

  (/=) x y = not (x == y)

instance showKey :: (Show a) => Show (Key a) where
  show (Key a) = "(Key " <> show a <> ")"
  show NoKey = "NoKey"

instance semigroupKey :: Semigroup (Key a) where
  (<>) k NoKey = k
  (<>) _ k = k

instance ordKey :: (Ord a) => Ord (Key a) where
  compare NoKey _ = LT
  compare _ NoKey = GT
  compare (Key a) (Key b) = compare a b

instance monoidKey :: Monoid (Key a) where
  mempty = NoKey

instance measuredElemKey :: Measured (Elem a) (Key a) where
  measure (Elem x) = Key x

-- | An ordered sequence.
newtype OrdSeq a
  = OrdSeq (FT.FingerTree (Key a) (Elem a))

partition :: forall a. (Ord a) => a -> OrdSeq a -> Tuple (OrdSeq a) (OrdSeq a)
partition k (OrdSeq xs) = Tuple (OrdSeq (force l)) (OrdSeq (force r))
  where
  t = FT.split (\y -> y >= Key k) xs
  l = fst t
  r = snd t

insert :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
insert x (OrdSeq xs) = OrdSeq (FT.append (force l) (FT.cons (Elem x) (force r)))
  where
  t = FT.split (\y -> y >= Key x) xs
  l = fst t
  r = snd t

deleteAll :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
deleteAll x (OrdSeq xs) = OrdSeq (l <> r')
  where
  t = FT.split (\y -> y >= Key x) xs
  l = force (fst t)
  r = force (snd t)
  t' = FT.split (\y -> y > Key x) r
  r' = force (snd t')

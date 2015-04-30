
-- | This module defines a sequence where elements are always kept in
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
import Data.Monoid.Additive
import Data.Foldable
import Data.Traversable
import Data.Unfoldable

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

-- `fmap OrdSeq` is a no-op, since OrdSeq is a newtype. Use this function
-- instead to avoid an unnecessary traversal of the structure.
fmapOrdSeq :: forall f a. (Functor f) => f (OrdSeqInner a) -> f (OrdSeq a)
fmapOrdSeq = unsafeCoerce

-- | An ordered sequence. The Semigroup instance uses the `merge` function.
newtype OrdSeq a
  = OrdSeq (OrdSeqInner a)

type OrdSeqInner a = FT.FingerTree (Key a) (Elem a)

empty :: forall a. OrdSeq a
empty = OrdSeq FT.Empty

instance eqOrdSeq :: (Eq a) => Eq (OrdSeq a) where
  (==) xs ys =
    if length xs == length ys
      then fromOrdSeq xs == (fromOrdSeq ys :: Array a)
      else false

  (/=) x y = not (x == y)

instance semigroupOrdSeq :: (Ord a) => Semigroup (OrdSeq a) where
  (<>) = merge

instance monoidOrdSeq :: (Ord a) => Monoid (OrdSeq a) where
  mempty = empty

instance foldableOrdSeq :: Foldable OrdSeq where
  foldr f z (OrdSeq xs) = foldr (liftElem f) z xs
  foldl f z (OrdSeq xs) = foldl (lift2Elem f) z xs
  foldMap f (OrdSeq xs) = foldMap (liftElem f) xs

{-- instance traversableOrdSeq :: Traversable OrdSeq where --}
{--   traverse f (OrdSeq xs) = fmapOrdSeq (traverse (traverse f) xs) --}
{--   sequence = traverse id --}

-- | O(1). Returns true if the given sequence is empty, false otherwise.
null :: forall a. OrdSeq a -> Boolean
null (OrdSeq FT.Empty) = true
null _ = false

-- | O(n). Return the length of the sequence.
length :: forall a. OrdSeq a -> Number
length = runAdditive <<< foldMap (const (Additive 1))

-- | O(log(n)). Split an ordered sequence into two halves. The first element
-- | of the returned tuple contains all elements which compared less than or
-- | equal to the argument; the second element contains the rest.
partition :: forall a. (Ord a) => a -> OrdSeq a -> Tuple (OrdSeq a) (OrdSeq a)
partition k (OrdSeq xs) = Tuple (OrdSeq (force l)) (OrdSeq (force r))
  where
  t = FT.split (\y -> y >= Key k) xs
  l = fst t
  r = snd t

-- | O(log(n)). Insert the given value into the correct place in the sequence.
insert :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
insert x (OrdSeq xs) = OrdSeq (FT.append (force l) (FT.cons (Elem x) (force r)))
  where
  t = FT.split (\y -> y >= Key x) xs
  l = fst t
  r = snd t

-- | O(log(n)). Delete all elements from the sequence which compare EQ to the
-- | given value.
deleteAll :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
deleteAll x (OrdSeq xs) = OrdSeq (l <> r')
  where
  t = FT.split (\y -> y >= Key x) xs
  l = force (fst t)
  r = force (snd t)
  t' = FT.split (\y -> y > Key x) r
  r' = force (snd t')

-- | O(m*log(n/m)), where m and n are the lengths of the longer and shorter
-- | sequences respectively. Create a new sequence containing every element
-- | in both of the given sequences.
merge :: forall a. (Ord a) => OrdSeq a -> OrdSeq a -> OrdSeq a
merge (OrdSeq xs) (OrdSeq ys) = OrdSeq (merge' xs ys)
  where
  merge' as bs =
    case FT.viewL bs of
      FT.NilL        -> as
      FT.ConsL a bs' ->
        let t = FT.split (\c -> c > measure a) as
            l = force (fst t)
            r = force (snd t)
        in l <> (FT.cons a (merge' (force bs') r))

-- | O(1). Access the least element of the sequence, or Nothing if the sequence
-- | is empty.
least :: forall a. (Ord a) => OrdSeq a -> Maybe a
least (OrdSeq xs) =
  case FT.viewL xs of
    FT.NilL      -> Nothing
    FT.ConsL x _ -> Just (getElem x)

-- | O(1). Access the least element of the sequence, or Nothing if the sequence
-- | is empty.
greatest :: forall a. (Ord a) => OrdSeq a -> Maybe a
greatest (OrdSeq xs) =
  case FT.viewR xs of
    FT.NilR      -> Nothing
    FT.SnocR _ x -> Just (getElem x)

-- | Probably O(n), but depends on the Foldable instance. Consruct an ordered
-- | sequence from any any `Foldable`.
toOrdSeq :: forall f a. (Foldable f, Ord a) => f a -> OrdSeq a
toOrdSeq = foldr insert empty

-- | Probably O(n), but depends on the Unfoldable instance. Unfold an ordered
-- | sequence in ascending order.
fromOrdSeq :: forall f a. (Functor f, Unfoldable f) => OrdSeq a -> f a
fromOrdSeq (OrdSeq xs) = fmapGetElem (FT.unfoldLeft xs)

-- | Probably O(n), but depends on the Unfoldable instance. Unfold an ordered
-- | sequence in descending order.
fromOrdSeqDescending :: forall f a. (Functor f, Unfoldable f) => OrdSeq a -> f a
fromOrdSeqDescending (OrdSeq xs) = fmapGetElem (FT.unfoldRight xs)

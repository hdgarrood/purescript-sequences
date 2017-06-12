
-- | This module contains a type, `Seq`, much like that from `Data.Sequence`,
-- | but which is guaranteed to contain at least one element.
-- |
-- | This module is intended to be imported qualified, to avoid name clashes or
-- | ambiguity. For example:
-- |
-- | ```purescript
-- | import Data.Sequence.NonEmpty as NonEmpty
-- | ```

module Data.Sequence.NonEmpty
  ( Seq(..)

  -- construction
  , singleton
  , cons
  , snoc
  , append

  -- queries
  , length
  , inBounds

  -- deconstruction
  , uncons
  , unsnoc
  , head
  , tail
  , init
  , last

  , toPlain
  , splitAt
  , take
  , drop
  , filter

  -- indexing
  , index
  , adjust
  , replace

  -- other
  , toUnfoldable
  ) where

import Prelude

import Control.Alt (class Alt)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Maybe (Maybe(Just, Nothing), maybe, fromJust)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(Tuple), fst, uncurry)
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartial)

import Data.Sequence as S

-- | A sequence which is guaranteed to contain at least one element.
data Seq a
  = Seq a (S.Seq a)

-- | O(1). Construct a sequence from a single element.
singleton :: forall a. a -> Seq a
singleton x = Seq x S.empty

-- | O(1). Add an element to the left end of a sequence.
cons :: forall a. a -> Seq a -> Seq a
cons x (Seq y ys) = Seq x (S.cons y ys)

-- | O(1). Add an element to the right end of a sequence.
snoc :: forall a. Seq a -> a -> Seq a
snoc (Seq x xs) y = Seq x (S.snoc xs y)

-- | O(log(min(i,n-i))). Join two sequence values together.
append :: forall a. Seq a -> Seq a -> Seq a
append (Seq x xs) (Seq y ys) = Seq x ((S.snoc xs y) <> ys)

-- | O(1). The number of elements in the sequence.
length :: forall a. Seq a -> Int
length (Seq _ xs) = S.length xs + 1

-- | O(1). True if the given index specifies an element that exists in the
-- | sequence, false otherwise.
inBounds :: forall a. Int -> Seq a -> Boolean
inBounds 0 _ = true
inBounds i (Seq _ xs) = S.inBounds (i - 1) xs

-- | O(1). Take one element off the left side of a Seq and return it, together
-- | with the (possibly empty) remainder of the Seq.
uncons :: forall a. Seq a -> Tuple a (S.Seq a)
uncons (Seq x xs) = Tuple x xs

-- | O(1). Take one element off the right side of a Seq and return it, together
-- | with the (possibly empty) remainder of the Seq.
unsnoc :: forall a. Seq a -> Tuple (S.Seq a) a
unsnoc (Seq x xs) =
  case S.unsnoc xs of
    Nothing           -> Tuple S.empty x
    Just (Tuple ys y) -> Tuple (S.cons x ys) y

-- | O(1). Get the first element of a non-empty sequence. Equivalent to
-- | `index 0`.
head :: forall a. Seq a -> a
head (Seq x _) = x

-- | O(1). Get all but the first element of a non-empty sequence. The returned
-- | sequence is possibly empty. Equivalent to `drop 1`.
tail :: forall a. Seq a -> S.Seq a
tail (Seq _ xs) = xs

-- | O(1). Get all but the last element of a non-empty sequence. Possibly empty.
init :: forall a. Seq a -> S.Seq a
init = fst <<< unsnoc

-- | O(1). Get the last element of a non-empty sequence.
last :: forall a. Seq a -> a
last (Seq x xs) = maybe x id (S.last xs)

-- | O(1). Turn a non-empty sequence into a "plain" sequence (i.e. one from
-- | Data.Sequence), containing the same elements.
toPlain :: Seq ~> S.Seq
toPlain (Seq x xs) = S.cons x xs

-- | O(log(min(i,n-i))). Split the sequence into two (possibly empty) subsequences.
-- | The first subsequence will have i elements (unless there are not that many in
-- | the whole sequence, in which case the first element is the same sequence,
-- | unchanged).
splitAt :: forall a. Int -> Seq a -> Tuple (S.Seq a) (S.Seq a)
splitAt i = S.splitAt i <<< toPlain

-- | O(log(min(i,n-i))). Take a certain number of values from the left end of
-- | a sequence, and discard the rest, returning a possibly empty sequence.
take :: forall a. Int -> Seq a -> S.Seq a
take i = S.take i <<< toPlain

-- | O(log(min(i,n-i))). Discard a given number of elements from the left end
-- | of a sequence, returning a possibly empty sequence.
drop :: forall a. Int -> Seq a -> S.Seq a
drop i = S.drop i <<< toPlain

-- | O(n). Create a new (possibly empty) sequence which contains only those
-- | elements of the input sequence which satisfy the given predicate.
filter :: forall a. (a -> Boolean) -> Seq a -> S.Seq a
filter p = S.filter p <<< toPlain

-- | O(log(min(i,n-i))). Retrieve the element at the given index in the
-- | sequence. This function is zero-based; that is, the first element in a
-- | sequence `xs` can be retrieved with `index 0 xs`.
index :: forall a. Int -> Seq a -> Maybe a
index 0 (Seq x _)  = Just x
index i (Seq _ xs) = S.index (i - 1) xs

-- | O(log(min(i,n-i))). Adjust the element at the specified index by
-- | applying the given function to it. If the index is out of range, the
-- | sequence is returned unchanged.
adjust :: forall a. (a -> a) -> Int -> Seq a -> Seq a
adjust f 0 (Seq x xs) = Seq (f x) xs
adjust f i (Seq x xs) = Seq x (S.adjust f (i - 1) xs)

-- | O(log(min(i,n-i))). Replace the element at the specified index with
-- | a new element. If the index is out of range, the sequence is returned
-- | unchanged.
replace :: forall a. a -> Int -> Seq a -> Seq a
replace x = adjust (const x)

-- | Probably O(n), but depends on the Unfoldable instance. Turn a `Seq` into
-- | any `Unfoldable`.
toUnfoldable :: forall f. Functor f => Unfoldable f => Seq ~> f
toUnfoldable = S.toUnfoldable <<< toPlain

fromPlain :: Partial => S.Seq ~> Seq
fromPlain = S.uncons >>> fromJust >>> uncurry Seq

instance showSeq :: (Show a) => Show (Seq a) where
  show (Seq x xs) = "(Seq " <> show x <> " " <> show xs <> ")"

instance eqSeq :: (Eq a) => Eq (Seq a) where
  eq (Seq x xs) (Seq y ys) = x == y && xs == ys

instance ordSeq :: (Ord a) => Ord (Seq a) where
  compare (Seq x xs) (Seq y ys) =
    case compare x y of
      EQ    -> compare xs ys
      other -> other

instance functorSeq :: Functor Seq where
  map f (Seq x xs) = Seq (f x) (f <$> xs)

instance applySeq :: Apply Seq where
  apply fs xs = unsafePartial $ fromPlain (toPlain fs <*> toPlain xs)

instance applicativeSeq :: Applicative Seq where
  pure x = Seq x S.empty

instance bindSeq :: Bind Seq where
  bind xs f = unsafePartial $ fromPlain (toPlain xs >>= (toPlain <<< f))

instance monadSeq :: Monad Seq

instance semigroupSeq :: Semigroup (Seq a) where
  append (Seq x xs) (Seq y ys) = Seq x ((S.snoc xs y) <> ys)

instance altSeq :: Alt Seq where
  alt = (<>)

instance foldableSeq :: Foldable Seq where
  foldr f z = toPlain >>> foldr f z
  foldl f z = toPlain >>> foldl f z
  foldMap f = toPlain >>> foldMap f

instance traversableSeq :: Traversable Seq where
  sequence   = unsafePartial $ toPlain >>> sequence   >>> map fromPlain
  traverse f = unsafePartial $ toPlain >>> traverse f >>> map fromPlain

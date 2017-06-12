
-- | This module provides a sequence data type, intended for the same sort of
-- | tasks as an Array would be in JavaScript, except with better asymptotic
-- | complexity for many operations.
-- |
-- | The implementation uses 2-3 finger trees annotated with sizes, as
-- | described in the paper [_Finger Trees: A Simple General-Purpose Data
-- | Structure_][1], Ralf Hinze and Ross Paterson, Journal of Functional
-- | Programming 16:2 (2006) pp 197-217.
-- |
-- | This module is intended to be imported qualified, to avoid name clashes.
-- | For example:
-- |
-- | ```purescript
-- | import Data.Sequence (Seq)
-- | import Data.Sequence as Seq
-- | ```
-- |
-- | [1]: http://staff.city.ac.uk/~ross/papers/FingerTree.pdf

module Data.Sequence
  ( Seq()

  -- construction
  , empty
  , singleton
  , cons
  , snoc
  , append
  , map
  , concat
  , concatMap
  , fromFoldable

  -- queries
  , length
  , null
  , inBounds

  -- deconstruction
  , uncons
  , unsnoc
  , head
  , tail
  , init
  , last

  , splitAt
  , take
  , drop
  , filter
  , sort

  -- indexing
  , index
  , adjust
  , replace

  -- other
  , toUnfoldable
  , fullyForce
  ) where

import Prelude hiding (append, map)

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Lazy (Lazy(), force)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (class Monoid)
import Data.Monoid.Additive (Additive(Additive))
import Data.Newtype (unwrap, un)
import Data.Profunctor.Strong ((***))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import Data.Sequence.Internal (Elem(Elem), mapGetElem, getElem, liftElem,
                               lift2Elem, measure, strJoin)
import Data.FingerTree as FT
import Data.Sequence.Ordered as Ordered

-- TODO: Optimise Apply instance (see Hackage)
-- TODO: adjust might be suboptimal, see Data.Sequence on Hackage
-- TODO: fromFoldable can be improved. See Hackage

type SeqInner a = FT.FingerTree (Additive Int) (Elem a)
newtype Seq a = Seq (SeqInner a)

-- `map Seq` is a no-op, since Seq is a newtype. Use this function instead
-- to avoid an unnecessary traversal of the structure.
mapSeq :: forall f a. (Functor f) => f (SeqInner a) -> f (Seq a)
mapSeq = unsafeCoerce

instance ordSeq :: (Ord a) => Ord (Seq a) where
  compare (Seq xs) (Seq ys) = FT.compareFingerTree xs ys

instance eqSeq :: (Eq a) => Eq (Seq a) where
  eq (Seq xs) (Seq ys) = FT.eqFingerTree xs ys

instance showSeq :: (Show a) => Show (Seq a) where
  show xs = "(Seq.fromFoldable [" <> strJoin "," (toUnfoldable xs) <> "])"

instance semigroupSeq :: Semigroup (Seq a) where
  append = append

instance monoidSeq :: Monoid (Seq a) where
  mempty = empty

instance foldableSeq :: Foldable Seq where
  foldr f z (Seq xs) = foldr (liftElem f) z xs
  foldl f z (Seq xs) = foldl (lift2Elem f) z xs
  foldMap f (Seq xs) = foldMap (liftElem f) xs

instance traversableSeq :: Traversable Seq where
  traverse f (Seq xs) = mapSeq (traverse (traverse f) xs)
  sequence = traverse id

instance unfoldableSeq :: Unfoldable Seq where
  unfoldr f xs = case f xs of
                  Just (Tuple x ys) -> cons x (unfoldr f ys)
                  Nothing           -> empty

instance functorSeq :: Functor Seq where
  map = map

instance applySeq :: Apply Seq where
  apply = ap

instance applicativeSeq :: Applicative Seq where
  pure = singleton

instance bindSeq :: Bind Seq where
  bind = flip concatMap

instance monadSeq :: Monad Seq

instance altSeq :: Alt Seq where
  alt = append

instance plusSeq :: Plus Seq where
  empty = empty

instance alternativeSeq :: Alternative Seq

instance monadPlusSeq :: MonadPlus Seq

instance monadZeroSeq :: MonadZero Seq

-- | A sequence with no elements.
empty :: forall a. Seq a
empty = Seq FT.Empty

-- | O(1). Create a Seq with one element.
singleton :: forall a. a -> Seq a
singleton x = cons x empty

-- | O(1). Add an element to the left end of a Seq.
cons :: forall a. a -> Seq a -> Seq a
cons x (Seq xs) = Seq (FT.cons (Elem x) xs)

-- | O(1). Add an element to the right end of a Seq.
snoc :: forall a. Seq a -> a -> Seq a
snoc (Seq xs) x = Seq (FT.snoc xs (Elem x))

-- | O(log(min(n1,n2)), where n1 and n2 are the lengths of the arguments. Join
-- | two Seqs together.
append :: forall a. Seq a -> Seq a -> Seq a
append (Seq a) (Seq b) = Seq (FT.append a b)

-- | O(m*log(n)), where m is the number of sequences, and n is the length of
-- | the longest sequence within it. Flatten a sequence of sequences.
concat :: forall a. Seq (Seq a) -> Seq a
concat = foldr append empty

-- | O(m*n), where m is the number of sequences, and n is the length of the
-- | longest sequence within it. Map a function over a sequence and then
-- | flatten the results.
concatMap :: forall a b. (a -> Seq b) -> Seq a -> Seq b
concatMap f = concat <<< map f

-- | O(1). The number of elements in the sequence.
length :: forall a. Seq a -> Int
length (Seq xs) = un Additive (measure xs)

-- | O(1). True if the sequence has no elements, false otherwise.
null :: forall a. Seq a -> Boolean
null (Seq FT.Empty) = true
null _              = false

-- | O(1). If the sequence is nonempty, take one element off its left side and
-- | return that together with the rest of the original sequence. Otherwise,
-- | return Nothing.
uncons :: forall a. Seq a -> Maybe (Tuple a (Seq a))
uncons (Seq xs) =
  case FT.viewL xs of
      FT.NilL       -> Nothing
      FT.ConsL y ys -> Just (Tuple (getElem y) (Seq (force ys)))

-- | O(1). If the sequence is nonempty, take one element off its right side and
-- | return that together with the rest of the original sequence. Otherwise,
-- | return Nothing.
unsnoc :: forall a. Seq a -> Maybe (Tuple (Seq a) a)
unsnoc (Seq xs) =
  case FT.viewR xs of
      FT.NilR       -> Nothing
      FT.SnocR ys y -> Just (Tuple (Seq (force ys)) (getElem y))

splitAt' :: forall a. Int -> Seq a -> Tuple (Lazy (Seq a)) (Lazy (Seq a))
splitAt' i (Seq xs) = seqify tuple
  where
  tuple = unsafePartial $ FT.split (\n -> i < unwrap n) xs

  seqify :: forall f. (Functor f) =>
    Tuple (f (SeqInner a)) (f (SeqInner a)) -> Tuple (f (Seq a)) (f (Seq a))
  seqify = unsafeCoerce

-- | O(log(min(i,n-i))). Split the sequence into two subsequences. The first
-- | subsequence will have i elements (unless there are not that many in the
-- | whole sequence, in which case the first element is the same sequence,
-- | unchanged).
splitAt :: forall a. Int -> Seq a -> Tuple (Seq a) (Seq a)
splitAt i xs = forceBoth tuple
  where
  forceBoth = force *** force
  tuple = splitAt' i xs

-- | O(log(min(i,n-i))). Take a certain number of values from the left end of
-- | a sequence, and discard the rest.
take :: forall a. Int -> Seq a -> Seq a
take i = force <<< fst <<< splitAt' i

-- | O(log(min(i,n-i))). Discard a given number of elements from the left side
-- | of a Seq.
drop :: forall a. Int -> Seq a -> Seq a
drop i = force <<< snd <<< splitAt' i

-- | O(1). True if the given index specifies an element that exists in the
-- | sequence, false otherwise.
inBounds :: forall a. Int -> Seq a -> Boolean
inBounds i seq = 0 <= i && i < length seq

-- | O(log(min(i,n-i))). Retrieve the element at the given index in the
-- | sequence. This function is zero-based; that is, the first element in a
-- | sequence `xs` can be retrieved with `index 0 xs`.
index :: forall a. Int -> Seq a -> Maybe a
index i xs =
  if inBounds i xs
    then unsafePartial $ Just $ unsafeIndex i xs
    else Nothing

-- | O(log(min(i,n-i))). Like `index`, but this function will throw an error
-- | instead of returning Nothing if the index is out of bounds.
unsafeIndex :: forall a. Partial => Int -> Seq a -> a
unsafeIndex i (Seq xs) =
  case FT.splitTree (\n -> i < unwrap n) (Additive 0) xs of
    FT.LazySplit _ x _ -> getElem x

-- | O(log(min(i,n-i))). Adjust the element at the specified index by
-- | applying the given function to it. If the index is out of range, the
-- | sequence is returned unchanged.
adjust :: forall a. (a -> a) -> Int -> Seq a -> Seq a
adjust f i xs =
  if inBounds i xs
    then unsafePartial $ unsafeAdjust f i xs
    else xs

-- | Adjust the element at a specified index. This function throws an error
-- | if the index supplied is out of bounds.
unsafeAdjust :: forall a. Partial => (a -> a) -> Int -> Seq a -> Seq a
unsafeAdjust f i (Seq xs) =
  case FT.splitTree (\n -> i < unwrap n) (Additive 0) xs of
    FT.LazySplit l x r ->
      let
        g :: Elem a -> Elem a
        g = unsafeCoerce f

        l' = FT.snoc (force l) (g x)
      in
        Seq (FT.append l' (force r))

-- | O(log(min(i,n-i))). Replace the element at the specified index with
-- | a new element. If the index is out of range, the sequence is returned
-- | unchanged.
replace :: forall a. a -> Int -> Seq a -> Seq a
replace x = adjust (const x)

-- | O(n). Apply a function to every element within a sequence. Note that this
-- | function is performed lazily &mdash; the actual call is almost
-- | instantaneous, regardless of the length of the sequence, because the
-- | function is not applied to all elements immediately. The eventual running
-- | time (assuming all elements are later requested) _is_ O(n), though.
map :: forall a b. (a -> b) -> Seq a -> Seq b
map f (Seq xs) = Seq (g <$> xs)
  where
  g :: Elem a -> Elem b
  g = unsafeCoerce f

-- | O(1). Get the first element of a Seq. Equivalent to `index 0`.
head :: forall a. Seq a -> Maybe a
head (Seq xs) = mapGetElem (FT.head xs)

-- | O(1). Get all but the first element of a Seq. Equivalent to `drop 1`.
tail :: forall a. Seq a -> Maybe (Seq a)
tail (Seq xs) = mapSeq (FT.tail xs)

-- | O(1). Get all but the last element of a Seq. Equivalent to `\seq -> take
-- | (length seq - 1)`.
init :: forall a. Seq a -> Maybe (Seq a)
init (Seq xs) = mapSeq (FT.init xs)

-- | O(1). Get the last element of a Seq. Equivalent to
-- | `\seq -> index (length seq - 1) seq`.
last :: forall a. Seq a -> Maybe a
last (Seq xs) = mapGetElem (FT.last xs)

-- | Probably O(n*log(n)), but depends on the Foldable instance. Turn any
-- | `Foldable` into a `Seq`.
fromFoldable :: forall f. Foldable f => f ~> Seq
fromFoldable = foldr cons empty

-- | Probably O(n), but depends on the Unfoldable instance. Turn a `Seq` into
-- | any `Unfoldable`.
toUnfoldable :: forall f. Functor f => Unfoldable f => Seq ~> f
toUnfoldable (Seq xs) = mapGetElem (FT.unfoldLeft xs)

-- | O(n). Create a new Seq which contains only those elements of the input
-- | Seq which satisfy the given predicate.
filter :: forall a. (a -> Boolean) -> Seq a -> Seq a
filter p (Seq xs) = Seq (FT.filter q xs)
  where
  q :: Elem a -> Boolean
  q = unsafeCoerce p

-- | O(n*log(n)). Sort the sequence, using the `sort` from
-- | `Data.Sequence.Ordered`. Note that this sorting algorithm is unstable.
sort :: forall a. Ord a => Seq a -> Seq a
sort = Ordered.sort

-- | Force evaluation of all unevaluated thunks within the sequence.
fullyForce :: forall a. Seq a -> Seq a
fullyForce (Seq xs) = Seq (FT.fullyForce xs)

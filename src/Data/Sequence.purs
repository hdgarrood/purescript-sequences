
-- | This module provides a Sequence data type, intended for the same sort of
-- | tasks as an Array would be in JavaScript, except with better asymptotic
-- | complexity for many operations.
-- |
-- | The implementation uses 2-3 finger trees annotated with sizes, as
-- | described in the paper [_Finger Trees: A Simple General-Purpose Data
-- | Structure_][1], Ralf Hinze and Ross Paterson, Journal of Functional
-- | Programming 16:2 (2006) pp 197-217.
-- |
-- | This module is intended to be imported qualified, to avoid name clashes or
-- | ambiguity. For example: `import qualified Data.Sequence as S`.
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
  , toSeq

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

  -- indexing
  , index
  , adjust
  , replace

  -- other
  , fromSeq
  ) where

import Prelude hiding (cons)

import Data.Lazy
import Data.Monoid
import Data.Monoid.Additive
import Data.Tuple
import Data.Maybe
import Data.Foldable
import Data.Unfoldable
import Data.Traversable
import Control.Alt
import Control.Plus (Plus)
import Control.Alternative
import Control.MonadPlus

import Data.Sequence.Internal
import qualified Data.FingerTree as FT

-- TODO: Optimise Eq instance, probably with lazy list
-- TODO: Optimise Apply instance (see Hackage)
-- TODO: adjust might be suboptimal, see Data.Sequence on Hackage
-- TODO: toSeq can be improved. See Hackage

type SeqInner a = FT.FingerTree (Additive Number) (Elem a)
newtype Seq a = Seq (SeqInner a)

-- `fmap Seq` is a no-op, since Seq is a newtype. Use this function instead
-- to avoid an unnecessary traversal of the structure.
fmapSeq :: forall f a. (Functor f) => f (SeqInner a) -> f (Seq a)
fmapSeq = unsafeCoerce

instance eqSeq :: (Eq a) => Eq (Seq a) where
  (==) xs ys = if length xs == length ys
                 then fromSeq xs == (fromSeq ys :: Array a)
                 else false
  (/=) xs ys = not (xs == ys)

instance showSeq :: (Show a) => Show (Seq a) where
  show xs = "toSeq [" <> strJoin "," (fromSeq xs) <> "]"

instance ordSeq :: (Ord a) => Ord (Seq a) where
  compare (Seq xs) (Seq ys) = FT.compareFingerTree xs ys

instance semigroupSeq :: Semigroup (Seq a) where
  (<>) = append

instance monoidSeq :: Monoid (Seq a) where
  mempty = empty

instance foldableSeq :: Foldable Seq where
  foldr f z (Seq xs) = foldr (liftElem f) z xs
  foldl f z (Seq xs) = foldl (lift2Elem f) z xs
  foldMap f (Seq xs) = foldMap (liftElem f) xs

instance traversableSeq :: Traversable Seq where
  traverse f (Seq xs) = fmapSeq (traverse (traverse f) xs)
  sequence = traverse id

instance unfoldableSeq :: Unfoldable Seq where
  unfoldr f xs = case f xs of
                  Just (Tuple x ys) -> cons x (unfoldr f ys)
                  Nothing           -> empty

instance functorSeq :: Functor Seq where
  (<$>) f (Seq xs) = Seq (g <$> xs)
    where
    g :: Elem a -> Elem b
    g = unsafeCoerce f

instance applySeq :: Apply Seq where
  (<*>) = ap

instance applicativeSeq :: Applicative Seq where
  pure = singleton

instance bindSeq :: Bind Seq where
  (>>=) xs f = foldl add empty xs
    where add ys x = append ys (f x)

instance monadSeq :: Monad Seq

instance altSeq :: Alt Seq where
  (<|>) = append

instance plusSeq :: Plus Seq where
  empty = empty

instance alternativeSeq :: Alternative Seq

instance monadPlusSeq :: MonadPlus Seq

-- | O(1). The number of elements in the sequence.
length :: forall a. Seq a -> Number
length (Seq xs) = runAdditive (measure xs)

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

splitAt' :: forall a. Number -> Seq a -> Tuple (Lazy (Seq a)) (Lazy (Seq a))
splitAt' i (Seq xs) = seqify tuple
  where
  tuple = FT.split (\n -> i < runAdditive n) xs

  seqify :: forall f. (Functor f) =>
    Tuple (f (SeqInner a)) (f (SeqInner a)) -> Tuple (f (Seq a)) (f (Seq a))
  seqify = unsafeCoerce

-- | O(log(min(i,n-i))). Split the sequence into two subsequences. The first
-- | subsequence will have i elements (unless there are not that many in the
-- | whole sequence, in which case the first element is the same sequence,
-- | unchanged).
splitAt :: forall a. Number -> Seq a -> Tuple (Seq a) (Seq a)
splitAt i xs = forceBoth tuple
  where
  forceBoth = force *** force
  tuple = splitAt' i xs

-- | O(log(min(i,n-i))). Take a certain number of values from the left end of
-- | a sequence, and discard the rest.
take :: forall a. Number -> Seq a -> Seq a
take i = force <<< fst <<< splitAt' i

-- | O(log(min(i,n-i))). Discard a given number of elements from the left side
-- | of a Seq.
drop :: forall a. Number -> Seq a -> Seq a
drop i = force <<< snd <<< splitAt' i

-- | O(1). True if the given index specifies an element that exists in the
-- | sequence, false otherwise.
inBounds :: forall a. Seq a -> Number -> Boolean
inBounds seq i = 0 <= i && i < length seq

-- | O(log(min(i,n-i))). Retrieve the element at the given index in the
-- | sequence. This function is zero-based; that is, the first element in a
-- | sequence `xs` can be retrieved with `index xs 0`.
index :: forall a. Seq a -> Number -> Maybe a
index xs i = if inBounds xs i then Just (unsafeIndex xs i) else Nothing

-- | O(log(min(i,n-i))). Like `index`, but this function will throw an error
-- | instead of returning Nothing if no element exists at the specified
-- | sequence.
unsafeIndex :: forall a. Seq a -> Number -> a
unsafeIndex (Seq xs) i =
  case FT.unsafeSplitTree (\n -> i < runAdditive n) (Additive 0) xs of
    FT.LazySplit _ x _ -> getElem x

-- | O(log(min(i,n-i))). Adjust the element at the specified index by
-- | applying the given function to it. If the index is out of range, the
-- | sequence is returned unchanged.
adjust :: forall a. (a -> a) -> Number -> Seq a -> Seq a
adjust f i xs = if inBounds xs i then unsafeAdjust f i xs else xs

unsafeAdjust :: forall a. (a -> a) -> Number -> Seq a -> Seq a
unsafeAdjust f i (Seq xs) =
  case FT.unsafeSplitTree (\n -> i < runAdditive n) (Additive 0) xs of
    FT.LazySplit l x r ->
      let
        g :: Elem a -> Elem a
        g = unsafeCoerce f

        l' = FT.cons (g x) (force l)
      in
        Seq (FT.append l' (force r))

-- | O(log(min(i,n-i))). Replace the element at the specified index with
-- | a new element. If the index is out of range, the sequence is returned
-- | unchanged.
replace :: forall a. a -> Number -> Seq a -> Seq a
replace x = adjust (const x)

-- | A sequence with no elements.
empty :: forall a. Seq a
empty = Seq FT.Empty

-- | O(1). Add an element to the left end of a Seq.
cons :: forall a. a -> Seq a -> Seq a
cons x (Seq xs) = Seq (FT.cons (Elem x) xs)

-- | O(1). Add an element to the right end of a Seq.
snoc :: forall a. Seq a -> a -> Seq a
snoc (Seq xs) x = Seq (FT.snoc xs (Elem x))

-- | O(1). Create a Seq with one element.
singleton :: forall a. a -> Seq a
singleton x = cons x empty

-- | O(log(min(i,n-i))). Join two Seqs together.
append :: forall a. Seq a -> Seq a -> Seq a
append (Seq a) (Seq b) = Seq (FT.append a b)

-- | O(1). Get the first element of a Seq. Equivalent to `\seq -> index seq 0`.
head :: forall a. Seq a -> Maybe a
head (Seq xs) = fmapGetElem (FT.head xs)

-- | O(1). Get all but the first element of a Seq. Equivalent to `drop 1`.
tail :: forall a. Seq a -> Maybe (Seq a)
tail (Seq xs) = fmapSeq (FT.tail xs)

-- | O(1). Get all but the last element of a Seq. Equivalent to `\seq -> take
-- | (length seq - 1)`.
init :: forall a. Seq a -> Maybe (Seq a)
init (Seq xs) = fmapSeq (FT.init xs)

-- | O(1). Get the last element of a Seq. Equivalent to
-- | `\seq -> index seq (length seq - 1)`.
last :: forall a. Seq a -> Maybe a
last (Seq xs) = fmapGetElem (FT.last xs)

-- | Probably O(n), but depends on the Foldable instance. Turn any `Foldable`
-- | into a `Seq`.
toSeq :: forall f a. (Foldable f) => f a -> Seq a
toSeq = foldr cons empty

-- | Probably O(n), but depends on the Unfoldable instance. Turn a `Seq` into
-- | any `Unfoldable`.
fromSeq :: forall f a. (Functor f, Unfoldable f) => Seq a -> f a
fromSeq (Seq xs) = fmapGetElem (FT.unfoldLeft xs)

-- | O(n). Create a new Seq which contains only those elements of the input
-- | Seq which satisfy the given predicate.
filter :: forall a. (a -> Boolean) -> Seq a -> Seq a
filter p (Seq xs) = Seq (FT.filter q xs)
  where
  q :: Elem a -> Boolean
  q = unsafeCoerce p

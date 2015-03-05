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

  -- indexing
  , index
  , adjust

  -- other
  , fromSeq
  ) where

import Prelude hiding (cons)

import Data.Lazy
import Data.Monoid
import Data.Tuple
import Data.Maybe
import Data.Foldable
import Data.Unfoldable
import Data.Traversable
import Control.Alt
import Control.Plus (Plus)
import Control.Alternative
import Control.MonadPlus

import qualified Data.FingerTree as FT
import Data.Sequence.Internal

-- TODO: Check safety of index
-- TODO: Optimise Eq instance, probably with lazy list
-- TODO: Optimise Apply instance (see Hackage)
-- TODO: adjust is unsafe
-- TODO: adjust might be suboptimal, see Data.Sequence on Hackage
-- TODO: toSeq can be improved. See Hackage

-- First: some utils
(***) :: forall a b aa bb. (a -> aa) -> (b -> bb) -> Tuple a b -> Tuple aa bb
(***) fa fb (Tuple a b) = Tuple (fa a) (fb b)

fmap :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
fmap = (<$>)

strJoin :: forall a. (Show a) => String -> Array a -> String
strJoin glue = intercalate glue <<< fmap show

-- With great power comes great responsibility. Always define an alias of
-- this with a type signature which is as specific as possible, never use it
-- directly.
foreign import unsafeCoerce """
  function unsafeCoerce(x) {
    return x
  } """ :: forall a b. a -> b

-- On to the main attraction
type SeqInner a = FT.FingerTree Size a
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
  foldr f z (Seq xs) = foldr f z xs
  foldl f z (Seq xs) = foldl f z xs
  foldMap f (Seq xs) = foldMap f xs

instance traversableSeq :: Traversable Seq where
  traverse f (Seq xs) = fmapSeq (traverse f xs)
  sequence = traverse id

instance unfoldableSeq :: Unfoldable Seq where
  unfoldr f xs = case f xs of
                  Just (Tuple x ys) -> cons x (unfoldr f ys)
                  Nothing           -> empty

instance functorSeq :: Functor Seq where
  (<$>) f (Seq xs) = Seq (f <$> xs)

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
length (Seq xs) = getSize (FT.measureTree' xs)

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
      FT.ConsL y ys -> Just (Tuple y (Seq (force ys)))

-- | O(1). If the sequence is nonempty, take one element off its right side and
-- | return that together with the rest of the original sequence. Otherwise,
-- | return Nothing.
unsnoc :: forall a. Seq a -> Maybe (Tuple (Seq a) a)
unsnoc (Seq xs) =
  case FT.viewR xs of
      FT.NilR       -> Nothing
      FT.SnocR ys y -> Just (Tuple (Seq (force ys)) y)

splitAt' :: forall a. Number -> Seq a -> Tuple (Lazy (Seq a)) (Lazy (Seq a))
splitAt' i (Seq xs) = seqify tuple
  where
  tuple = FT.split (\n -> i < getSize n) xs

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

-- | O(log(min(i,n-i))). Discard all elements from a Seq after the first n.
take :: forall a. Number -> Seq a -> Seq a
take i = force <<< fst <<< splitAt' i

-- | O(log(min(i,n-i))). Discard a given number of elements from the left side
-- | of a Seq.
drop :: forall a. Number -> Seq a -> Seq a
drop i = force <<< snd <<< splitAt' i

-- | O(log(min(i,n-i))). Retrieve the element at the given position in the 
-- | sequence. This function is zero-based; that is, the first element in a
-- | sequence `xs` can be retrieved with `index xs 0`.
index :: forall a. Seq a -> Number -> Maybe a
index (Seq xs) i
  | 0 <= i && i < (length (Seq xs)) =
    case FT.unsafeSplitTree (\n -> i < getSize n) (Size 0) xs of
      FT.LazySplit _ x _ -> Just x
  | otherwise = Nothing

-- | O(log(min(i,n-i))). Update the element at the specified position. If the
-- | position is out of range, the original sequence is returned.
adjust :: forall a. (a -> a) -> Number -> Seq a -> Seq a
adjust f i (Seq xs) =
  case FT.unsafeSplitTree (\n -> i < getSize n) (Size 0) xs of
    FT.LazySplit l x r ->
      let
        l' = FT.cons (f x) (force l)
      in
        Seq (FT.append l' (force r))

-- | A sequence with no elements.
empty :: forall a. Seq a
empty = Seq FT.Empty

-- | O(1). Add an element to the left end of a Seq.
cons :: forall a. a -> Seq a -> Seq a
cons x (Seq xs) = Seq (FT.cons x xs)

-- | O(1). Add an element to the right end of a Seq.
snoc :: forall a. Seq a -> a -> Seq a
snoc (Seq xs) x = Seq (FT.snoc xs x)

-- | O(1). Create a Seq with one element.
singleton :: forall a. a -> Seq a
singleton x = cons x empty

-- | O(log(min(i,n-i))). Join two Seqs together.
append :: forall a. Seq a -> Seq a -> Seq a
append (Seq a) (Seq b) = Seq (FT.append a b)

-- | O(1). Get the first element of a Seq. Equivalent to `\seq -> index seq 0`.
head :: forall a. Seq a -> Maybe a
head (Seq xs) = FT.head xs

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
last (Seq xs) = FT.last xs

-- | Probably O(n), but depends on the Foldable instance. Turn any `Foldable`
-- | into a `Seq`.
toSeq :: forall f a. (Foldable f) => f a -> Seq a
toSeq = foldr cons empty

-- | Probably O(n), but depends on the Unfoldable instance. Turn a `Seq` into
-- | any `Unfoldable`.
fromSeq :: forall f a. (Functor f, Unfoldable f) => Seq a -> f a
fromSeq (Seq xs) = FT.fromFingerTree xs


-- | This module defines a sequence where elements are always kept in
-- | order. This enables constant time access to the least and greatest
-- | elements, in addition to logarithmic time partitioning.
-- |
-- | The module is intended to be imported qualified, to avoid ambiguity or
-- | name clashes. For example, `import Data.Sequence.Ordered as OrdSeq`.

module Data.Sequence.Ordered
  ( OrdSeq()

  -- construction
  , empty
  , toOrdSeq
  , insert

  -- queries
  , null
  , length
  , least
  , greatest

  -- deconstruction
  , popLeast
  , popGreatest
  , partition

  -- combination
  , merge
  , intersection

  -- modification
  , deleteAll

  -- other
  , fromOrdSeq
  , fromOrdSeqDescending
  , sort
  ) where

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

-- TODO: there may be a better implementation for intersection.

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
  (==) (OrdSeq xs) (OrdSeq ys) = FT.eqFingerTree xs ys
  (/=) x y = not (x == y)

instance showOrdSeq :: (Show a) => Show (OrdSeq a) where
  show xs = "(toOrdSeq [" <> strJoin "," (fromOrdSeq xs) <> "])"

instance semigroupOrdSeq :: (Ord a) => Semigroup (OrdSeq a) where
  (<>) = merge

instance monoidOrdSeq :: (Ord a) => Monoid (OrdSeq a) where
  mempty = empty

instance foldableOrdSeq :: Foldable OrdSeq where
  foldr f z (OrdSeq xs) = foldr (liftElem f) z xs
  foldl f z (OrdSeq xs) = foldl (lift2Elem f) z xs
  foldMap f (OrdSeq xs) = foldMap (liftElem f) xs

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
merge (OrdSeq xs) (OrdSeq ys) = OrdSeq (go xs ys)
  where
  go as bs =
    case FT.viewL bs of
      FT.NilL        -> as
      FT.ConsL a bs' ->
        let t = FT.split (\c -> c > measure a) as
            l = force (fst t)
            r = force (snd t)
        in l <> (FT.cons a (go (force bs') r))

-- | O(n), where n is the length of the longer sequence. Create a new sequence
-- | containing only elements which are common to both sequences.
intersection :: forall a. (Ord a) => OrdSeq a -> OrdSeq a -> OrdSeq a
intersection (OrdSeq xs) (OrdSeq ys) = OrdSeq (go xs ys)
  where
  go as bs =
    case Tuple (FT.viewL as) (FT.viewL bs) of
      Tuple FT.NilL _ -> FT.Empty
      Tuple _ FT.NilL -> FT.Empty
      Tuple (FT.ConsL a as') (FT.ConsL b bs') ->
        case compare a b of
          LT -> go (force as') bs
          EQ -> FT.cons a (go (force as') (force bs'))
          GT -> go as (force bs')

-- | O(1). Access the least element of the sequence, or Nothing if the sequence
-- | is empty.
least :: forall a. (Ord a) => OrdSeq a -> Maybe a
least (OrdSeq xs) =
  case FT.viewL xs of
    FT.NilL      -> Nothing
    FT.ConsL x _ -> Just (getElem x)

-- | O(1). Remove the least element of the sequence, returning that element and
-- | the remainder of the sequence. If the sequence is empty, return Nothing.
popLeast :: forall a. (Ord a) => OrdSeq a -> Maybe (Tuple a (OrdSeq a))
popLeast (OrdSeq xs) =
  case FT.viewL xs of
    FT.NilL       -> Nothing
    FT.ConsL x xs -> Just (Tuple (getElem x) (OrdSeq (force xs)))

-- | O(1). Access the greatest element of the sequence, or Nothing if the
-- | sequence is empty.
greatest :: forall a. (Ord a) => OrdSeq a -> Maybe a
greatest (OrdSeq xs) =
  case FT.viewR xs of
    FT.NilR      -> Nothing
    FT.SnocR _ x -> Just (getElem x)

-- | O(1). Remove the greatest element of the sequence, returning that element
-- | and the remainder of the sequence. If the sequence is empty, return
-- | Nothing.
popGreatest :: forall a. (Ord a) => OrdSeq a -> Maybe (Tuple a (OrdSeq a))
popGreatest (OrdSeq xs) =
  case FT.viewR xs of
    FT.NilR       -> Nothing
    FT.SnocR xs x -> Just (Tuple (getElem x) (OrdSeq (force xs)))

-- | Probably O(n*log(n)), but depends on the Foldable instance. Consruct an
-- | ordered sequence from any any `Foldable`.
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

-- | Sort any structure (which has Foldable, Unfoldable, and Functor instances)
-- | by converting to an OrdSeq and back again. I am fairly sure this is
-- | usually O(n*log(n)), although of course this depends on the Unfoldable and
-- | Foldable instances.
sort :: forall f a. (Functor f, Foldable f, Unfoldable f, Ord a) => f a -> f a
sort = fromOrdSeq <<< toOrdSeq

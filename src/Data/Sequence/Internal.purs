module Data.Sequence.Internal
  ( (<$$>), mapmap
  , (<$$$>), mapmapmap
  , strJoin
  , class Measured
  , measure
  , Elem(..)
  , getElem
  , mapElem
  , mapGetElem
  , lift2Elem
  , liftElem
  , Key(..)
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl, intercalate)
import Data.Lazy (Lazy(), force)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(Additive))
import Data.Traversable (class Traversable)
import Unsafe.Coerce (unsafeCoerce)

-----------------------
-- Various utilities

mapmap :: forall f g a b. Functor f => Functor g =>
  (a -> b) -> f (g a) -> f (g b)
mapmap = (<$>) <<< (<$>)

infix 2 mapmap as <$$>

mapmapmap :: forall f g h a b. Functor f => Functor g => Functor h =>
  (a -> b) -> f (g (h a)) -> f (g (h b))
mapmapmap = (<$$>) <<< (<$>)

infix 2 mapmapmap as <$$$>

strJoin :: forall a. Show a => String -> Array a -> String
strJoin glue = intercalate glue <<< map show

----------------------------------------
-- FingerTree/Sequence specific stuff

class Measured a v where
  measure :: a -> v

instance measuredArray :: (Monoid v, Measured a v) => Measured (Array a) v where
  measure xs = foldl (\i a -> i <> measure a) mempty xs

instance measuredLazy :: (Monoid v, Measured a v) => Measured (Lazy a) v where
  measure s = measure (force s)

newtype Elem a = Elem a

getElem :: forall a. Elem a -> a
getElem (Elem a) = a

-- `map Elem` is a no-op, since Elem is a newtype. Use this function instead
-- to avoid an unnecessary traversal of the structure.
mapElem :: forall f a. Functor f => f a -> f (Elem a)
mapElem = unsafeCoerce

-- `map getElem` is a no-op, since Elem is a newtype. Use this function
-- instead to avoid an unnecessary traversal of the structure.
mapGetElem :: forall f a. Functor f => f (Elem a) -> f a
mapGetElem = unsafeCoerce

lift2Elem :: forall a b. (b -> a -> b) -> b -> Elem a -> b
lift2Elem = unsafeCoerce

liftElem :: forall a b. (a -> b) -> Elem a -> b
liftElem = unsafeCoerce

instance measuredElem :: Measured (Elem a) (Additive Int) where
  measure _ = Additive 1

instance showElem :: (Show a) => Show (Elem a) where
  show x = "Elem (" <> show (getElem x) <> ")"

instance eqElem :: (Eq a) => Eq (Elem a) where
  eq (Elem x) (Elem y) = x == y

instance ordElem :: (Ord a) => Ord (Elem a) where
  compare (Elem x) (Elem y) = compare x y

instance foldableElem :: Foldable Elem where
  foldr f z (Elem x) = f x z
  foldl f z (Elem x) = f z x
  foldMap f (Elem x) = f x

instance functorElem :: Functor Elem where
  map f (Elem x) = Elem (f x)

instance traversableElem :: Traversable Elem where
  traverse f (Elem x) = mapElem (f x)
  sequence (Elem fx)  = mapElem fx

data Key a = NoKey | Key a

instance eqKey :: (Eq a) => Eq (Key a) where
  eq (Key a) (Key b) = a == b
  eq NoKey NoKey     = true
  eq _ _             = false

instance showKey :: (Show a) => Show (Key a) where
  show (Key a) = "(Key " <> show a <> ")"
  show NoKey = "NoKey"

instance semigroupKey :: Semigroup (Key a) where
  append k NoKey = k
  append _ k = k

instance ordKey :: (Ord a) => Ord (Key a) where
  compare NoKey _ = LT
  compare _ NoKey = GT
  compare (Key a) (Key b) = compare a b

instance monoidKey :: Monoid (Key a) where
  mempty = NoKey

instance measuredElemKey :: Measured (Elem a) (Key a) where
  measure (Elem x) = Key x

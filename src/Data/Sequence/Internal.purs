
module Data.Sequence.Internal where

import Data.Monoid
import Data.Monoid.Additive
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Traversable

import qualified Data.FingerTree as FT

-----------------------
-- Various utilities

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

newtype Elem a = Elem a

getElem :: forall a. Elem a -> a
getElem (Elem a) = a

-- `fmap Elem` is a no-op, since Elem is a newtype. Use this function instead
-- to avoid an unnecessary traversal of the structure.
fmapElem :: forall f a. (Functor f) => f a -> f (Elem a)
fmapElem = unsafeCoerce

-- `fmap getElem` is a no-op, since Elem is a newtype. Use this function
-- instead to avoid an unnecessary traversal of the structure.
fmapGetElem :: forall f a. (Functor f) => f (Elem a) -> f a
fmapGetElem = unsafeCoerce

instance measuredElem :: FT.Measured (Elem a) (Additive Number) where
  measure _ = Additive 1

instance showElem :: (Show a) => Show (Elem a) where
  show x = "Elem (" <> show (getElem x) <> ")"

instance eqElem :: (Eq a) => Eq (Elem a) where
  (==) (Elem x) (Elem y) = x == y
  (/=) x y = not (x == y)

instance ordElem :: (Ord a) => Ord (Elem a) where
  compare (Elem x) (Elem y) = compare x y

instance foldableElem :: Foldable Elem where
  foldr f z (Elem x) = f x z
  foldl f z (Elem x) = f z x
  foldMap f (Elem x) = f x

instance functorElem :: Functor Elem where
  (<$>) f (Elem x) = Elem (f x)

instance traversableElem :: Traversable Elem where
  traverse f (Elem x) = fmapElem (f x)
  sequence (Elem fx)  = fmapElem fx

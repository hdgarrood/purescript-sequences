
module Data.Sequence.Internal where

import Data.Monoid
import Data.Monoid.Additive
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Traversable
import Data.Array
import Data.Lazy

-----------------------
-- Various utilities

(***) :: forall a b aa bb. (a -> aa) -> (b -> bb) -> Tuple a b -> Tuple aa bb
(***) fa fb (Tuple a b) = Tuple (fa a) (fb b)

fmap :: forall f a b. (Functor f) => (a -> b) -> f a -> f b
fmap = (<$>)

(<$$>) :: forall f g a b. (Functor f, Functor g) =>
  (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) <<< (<$>)

(<$$$>) :: forall f g h a b. (Functor f, Functor g, Functor h) =>
  (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = (<$$>) <<< (<$>)

strJoin :: forall a. (Show a) => String -> Array a -> String
strJoin glue = intercalate glue <<< fmap show

-- With great power comes great responsibility. Always define an alias of
-- this with a type signature which is as specific as possible, never use it
-- directly.
foreign import unsafeCoerce """
  function unsafeCoerce(x) {
    return x
  } """ :: forall a b. a -> b

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

-- `fmap Elem` is a no-op, since Elem is a newtype. Use this function instead
-- to avoid an unnecessary traversal of the structure.
fmapElem :: forall f a. (Functor f) => f a -> f (Elem a)
fmapElem = unsafeCoerce

-- `fmap getElem` is a no-op, since Elem is a newtype. Use this function
-- instead to avoid an unnecessary traversal of the structure.
fmapGetElem :: forall f a. (Functor f) => f (Elem a) -> f a
fmapGetElem = unsafeCoerce

lift2Elem :: forall a b. (b -> a -> b) -> b -> Elem a -> b
lift2Elem = unsafeCoerce

liftElem :: forall a b. (a -> b) -> Elem a -> b
liftElem = unsafeCoerce

instance measuredElem :: Measured (Elem a) (Additive Number) where
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

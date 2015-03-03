module Sequence where

import Data.Lazy
import Data.Monoid
import Data.Tuple
import Data.Maybe
import qualified Data.FingerTree as FT

newtype Size = Size Number

getSize :: Size -> Number
getSize (Size n) = n

instance semigrouSize :: Semigroup Size where
  (<>) m n = Size (getSize m + getSize n)

instance monoidSize :: Monoid Size where
  mempty = Size 0

instance showSize :: Show Size where
  show x = "(Size " <> show (getSize x) <> ")"

newtype Elem a = Elem a

getElem :: forall a. Elem a -> a
getElem (Elem a) = a

instance measuredElem :: FT.Measured (Elem a) Size where
  measure _ = Size 1

instance showElem :: (Show a) => Show (Elem a) where
  show x = "(Elem " ++ show (getElem x) <> ")"

type Seq a = FT.FingerTree Size (Elem a)

length :: forall a. Seq a -> Number
length xs = getSize (FT.measure xs)

splitAt :: forall a. Number -> Seq a -> Tuple (Lazy (Seq a)) (Lazy (Seq a))
splitAt i xs = FT.split (\n -> i < getSize n) xs

(!) :: forall a. Seq a -> Number -> a
(!) xs i =
  case FT.splitTree (\n -> i < getSize n) (Size 0) xs of
    FT.LazySplit _ x _ -> getElem x

empty :: forall a. Seq a
empty = FT.Empty

(<|) :: forall a. a -> Seq a -> Seq a
(<|) x xs = FT.(<|) (Elem x) xs

(|>) :: forall a. Seq a -> a -> Seq a
(|>) xs x = FT.(|>) xs (Elem x)

(><) :: forall a. Seq a -> Seq a -> Seq a
(><) = FT.(><)

headL :: forall a. Seq a -> Maybe a
headL xs = getElem <$> FT.headL xs

tailL :: forall a. Seq a -> Maybe (Seq a)
tailL = FT.tailL

-- TODO
-- other operations
-- other instance declarations

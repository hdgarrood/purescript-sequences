module Data.Sequence.Internal where

import Data.Monoid
import qualified Data.FingerTree as FT

newtype Size = Size Number

getSize :: Size -> Number
getSize (Size n) = n

instance semigroupSize :: Semigroup Size where
  (<>) m n = Size (getSize m + getSize n)

instance monoidSize :: Monoid Size where
  mempty = Size 0

instance showSize :: Show Size where
  show x = "Size (" <> show (getSize x) <> ")"

instance measuredSize :: FT.Measured a Size where
  measure _ = Size 1

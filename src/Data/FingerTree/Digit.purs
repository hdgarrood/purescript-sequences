module Data.FingerTree.Digit
  ( Digit
  , mkDigit
  , mkDigitMay
  , mkDigit1
  , mkDigit2
  , mkDigit3
  , runDigit
  , headDigit
  , tailDigit
  , initDigit
  , lastDigit
  , snocDigit
  , consDigit
  , dropDigit
  , digitLength
  , unsafeIndex
  , (!)
  ) where

import Prelude
import Data.Monoid (class Monoid)
import Data.Maybe (Maybe(..))
import Data.Array as A
import Data.Array.Partial as AP
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import Data.Sequence.Internal (class Measured, measure)

-- | A Digit is just an array which has between one and four elements
-- | (inclusive). If a Digit has two or three elements, it is described as
-- | 'safe'; otherwise, it is described as 'dangerous'.
newtype Digit a = Digit (Array a)

runDigit :: forall a. Digit a -> Array a
runDigit (Digit xs) = xs

overDigit :: forall a. (Array a -> Array a) -> (Digit a -> Digit a)
overDigit = unsafeCoerce

derive newtype instance functorDigit :: Functor Digit
derive newtype instance showDigit :: Show a => Show (Digit a)
derive newtype instance foldableDigit :: Foldable Digit
derive newtype instance traversableDigit :: Traversable Digit

instance measuredDigit :: (Monoid v, Measured a v) => Measured (Digit a) v where
  measure = measure <<< runDigit

-- TODO: This fails with 'cannot derive newtype instance ... make sure this is a newtype.'
-- derive newtype instance measuredDigit :: (Monoid v, Measured a v) => Measured (Digit a) v

-- TODO: The following (incorrect) code causes an ICE on 0.10.4. Report it.
-- derive newtype instance measuredDigit :: Measured a => Measured (Digit a)

mkDigit1 :: forall a. a -> Digit a
mkDigit1 x = Digit [x]

mkDigit2 :: forall a. a -> a -> Digit a
mkDigit2 x y = Digit [x, y]

mkDigit3 :: forall a. a -> a -> a -> Digit a
mkDigit3 x y z = Digit [x, y, z]

-- | This function is only defined when the argument has between one and four
-- | elements inclusive. It will not throw an error if this is not satisfied,
-- | although the Digit length invariant will be violated, which will cause
-- | other functions to break.
mkDigit :: forall a. Partial => Array a -> Digit a
mkDigit = Digit

-- | Like mkDigit, except this returns Nothing on invalid input.
mkDigitMay :: forall a. Array a -> Maybe (Digit a)
mkDigitMay xs =
  if between 1 4 (A.length xs)
    then Just (Digit xs)
    else Nothing

-- The following functions are total because a Digit is guaranteed to have at
-- least one element.

headDigit :: forall a. Digit a -> a
headDigit = unsafePartial AP.head <<< runDigit

tailDigit :: forall a. Digit a -> Array a
tailDigit = unsafePartial AP.tail <<< runDigit

lastDigit :: forall a. Digit a -> a
lastDigit = unsafePartial AP.last <<< runDigit

initDigit :: forall a. Digit a -> Array a
initDigit = unsafePartial AP.init <<< runDigit

dropDigit :: forall a. Int -> Digit a -> Array a
dropDigit n = A.drop n <<< runDigit

-- | Append a single element. This is partial because the result is not defined
-- | in the case where the argument has 4 elements.
snocDigit :: forall a. Partial => Digit a -> a -> Digit a
snocDigit dg x = overDigit (\xs -> A.snoc xs x) dg

-- | Prepend a single element. This is partial because the result is not defined
-- | in the case where the argument has 4 elements.
consDigit :: forall a. Partial => a -> Digit a -> Digit a
consDigit x dg = overDigit (A.cons x) dg

digitLength :: forall a. Digit a -> Int
digitLength = A.length <<< runDigit

unsafeIndex :: forall a. Partial => Digit a -> Int -> a
unsafeIndex xs = A.unsafeIndex (runDigit xs)

infix 2 unsafeIndex as !

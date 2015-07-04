## Module Data.Sequence

This module provides a sequence data type, intended for the same sort of
tasks as an Array would be in JavaScript, except with better asymptotic
complexity for many operations.

The implementation uses 2-3 finger trees annotated with sizes, as
described in the paper [_Finger Trees: A Simple General-Purpose Data
Structure_][1], Ralf Hinze and Ross Paterson, Journal of Functional
Programming 16:2 (2006) pp 197-217.

This module is intended to be imported qualified, to avoid name clashes or
ambiguity. For example: `import qualified Data.Sequence as S`.

[1]: http://staff.city.ac.uk/~ross/papers/FingerTree.pdf

#### `Seq`

``` purescript
newtype Seq a
```

##### Instances
``` purescript
instance ordSeq :: (Ord a) => Ord (Seq a)
instance eqSeq :: (Eq a) => Eq (Seq a)
instance showSeq :: (Show a) => Show (Seq a)
instance semigroupSeq :: Semigroup (Seq a)
instance monoidSeq :: Monoid (Seq a)
instance foldableSeq :: Foldable Seq
instance traversableSeq :: Traversable Seq
instance unfoldableSeq :: Unfoldable Seq
instance functorSeq :: Functor Seq
instance applySeq :: Apply Seq
instance applicativeSeq :: Applicative Seq
instance bindSeq :: Bind Seq
instance monadSeq :: Monad Seq
instance altSeq :: Alt Seq
instance plusSeq :: Plus Seq
instance alternativeSeq :: Alternative Seq
instance monadPlusSeq :: MonadPlus Seq
```

#### `empty`

``` purescript
empty :: forall a. Seq a
```

A sequence with no elements.

#### `singleton`

``` purescript
singleton :: forall a. a -> Seq a
```

O(1). Create a Seq with one element.

#### `cons`

``` purescript
cons :: forall a. a -> Seq a -> Seq a
```

O(1). Add an element to the left end of a Seq.

#### `snoc`

``` purescript
snoc :: forall a. Seq a -> a -> Seq a
```

O(1). Add an element to the right end of a Seq.

#### `append`

``` purescript
append :: forall a. Seq a -> Seq a -> Seq a
```

O(log(min(n1,n2)), where n1 and n2 are the lengths of the arguments. Join
two Seqs together.

#### `concat`

``` purescript
concat :: forall a. Seq (Seq a) -> Seq a
```

O(m*log(n)), where m is the number of sequences, and n is the length of
the longest sequence within it. Flatten a sequence of sequences.

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> Seq b) -> Seq a -> Seq b
```

O(m*n), where m is the number of sequences, and n is the length of the
longest sequence within it. Map a function over a sequence and then
flatten the results.

#### `length`

``` purescript
length :: forall a. Seq a -> Int
```

O(1). The number of elements in the sequence.

#### `null`

``` purescript
null :: forall a. Seq a -> Boolean
```

O(1). True if the sequence has no elements, false otherwise.

#### `uncons`

``` purescript
uncons :: forall a. Seq a -> Maybe (Tuple a (Seq a))
```

O(1). If the sequence is nonempty, take one element off its left side and
return that together with the rest of the original sequence. Otherwise,
return Nothing.

#### `unsnoc`

``` purescript
unsnoc :: forall a. Seq a -> Maybe (Tuple (Seq a) a)
```

O(1). If the sequence is nonempty, take one element off its right side and
return that together with the rest of the original sequence. Otherwise,
return Nothing.

#### `splitAt`

``` purescript
splitAt :: forall a. Int -> Seq a -> Tuple (Seq a) (Seq a)
```

O(log(min(i,n-i))). Split the sequence into two subsequences. The first
subsequence will have i elements (unless there are not that many in the
whole sequence, in which case the first element is the same sequence,
unchanged).

#### `take`

``` purescript
take :: forall a. Int -> Seq a -> Seq a
```

O(log(min(i,n-i))). Take a certain number of values from the left end of
a sequence, and discard the rest.

#### `drop`

``` purescript
drop :: forall a. Int -> Seq a -> Seq a
```

O(log(min(i,n-i))). Discard a given number of elements from the left side
of a Seq.

#### `inBounds`

``` purescript
inBounds :: forall a. Int -> Seq a -> Boolean
```

O(1). True if the given index specifies an element that exists in the
sequence, false otherwise.

#### `index`

``` purescript
index :: forall a. Int -> Seq a -> Maybe a
```

O(log(min(i,n-i))). Retrieve the element at the given index in the
sequence. This function is zero-based; that is, the first element in a
sequence `xs` can be retrieved with `index 0 xs`.

#### `adjust`

``` purescript
adjust :: forall a. (a -> a) -> Int -> Seq a -> Seq a
```

O(log(min(i,n-i))). Adjust the element at the specified index by
applying the given function to it. If the index is out of range, the
sequence is returned unchanged.

#### `replace`

``` purescript
replace :: forall a. a -> Int -> Seq a -> Seq a
```

O(log(min(i,n-i))). Replace the element at the specified index with
a new element. If the index is out of range, the sequence is returned
unchanged.

#### `map`

``` purescript
map :: forall a b. (a -> b) -> Seq a -> Seq b
```

O(n). Apply a function to every element within a sequence. Note that this
function is performed lazily &mdash; the actual call is almost
instantaneous, regardless of the length of the sequence, because the
function is not applied to all elements immediately. The eventual running
time (assuming all elements are later requested) _is_ O(n), though.

#### `head`

``` purescript
head :: forall a. Seq a -> Maybe a
```

O(1). Get the first element of a Seq. Equivalent to `index 0`.

#### `tail`

``` purescript
tail :: forall a. Seq a -> Maybe (Seq a)
```

O(1). Get all but the first element of a Seq. Equivalent to `drop 1`.

#### `init`

``` purescript
init :: forall a. Seq a -> Maybe (Seq a)
```

O(1). Get all but the last element of a Seq. Equivalent to `\seq -> take
(length seq - 1)`.

#### `last`

``` purescript
last :: forall a. Seq a -> Maybe a
```

O(1). Get the last element of a Seq. Equivalent to
`\seq -> index (length seq - 1) seq`.

#### `toSeq`

``` purescript
toSeq :: forall f a. (Foldable f) => f a -> Seq a
```

Probably O(n*log(n)), but depends on the Foldable instance. Turn any
`Foldable` into a `Seq`.

#### `fromSeq`

``` purescript
fromSeq :: forall f a. (Functor f, Unfoldable f) => Seq a -> f a
```

Probably O(n), but depends on the Unfoldable instance. Turn a `Seq` into
any `Unfoldable`.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> Seq a -> Seq a
```

O(n). Create a new Seq which contains only those elements of the input
Seq which satisfy the given predicate.

#### `sort`

``` purescript
sort :: forall a. (Ord a) => Seq a -> Seq a
```

O(n*log(n)). Sort the sequence, using the `sort` from
`Data.Sequence.Ordered`. Note that this sorting algorithm is unstable.

#### `fullyForce`

``` purescript
fullyForce :: forall a. Seq a -> Seq a
```

Force evaluation of all unevaluated thunks within the sequence.



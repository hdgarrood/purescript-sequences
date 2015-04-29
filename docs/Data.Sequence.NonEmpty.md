# Module Documentation

## Module Data.Sequence.NonEmpty


This module contains a type, `Seq`, much like that from `Data.Sequence`,
but which is guaranteed to contain at least one element.

This module is intended to be imported qualified, to avoid name clashes or
ambiguity. For example: `import qualified Data.Sequence.NonEmpty as NES`.

#### `Seq`

``` purescript
data Seq a
  = Seq a (S.Seq a)
```

A sequence which is guaranteed to contain at least one element.

#### `singleton`

``` purescript
singleton :: forall a. a -> Seq a
```

O(1). Construct a sequence from a single element.

#### `cons`

``` purescript
cons :: forall a. a -> Seq a -> Seq a
```

O(1). Add an element to the left end of a sequence.

#### `snoc`

``` purescript
snoc :: forall a. Seq a -> a -> Seq a
```

O(1). Add an element to the right end of a sequence.

#### `append`

``` purescript
append :: forall a. Seq a -> Seq a -> Seq a
```

O(log(min(i,n-i))). Join two sequence values together.

#### `length`

``` purescript
length :: forall a. Seq a -> Number
```

O(1). The number of elements in the sequence.

#### `inBounds`

``` purescript
inBounds :: forall a. Seq a -> Number -> Boolean
```

O(1). True if the given index specifies an element that exists in the
sequence, false otherwise.

#### `uncons`

``` purescript
uncons :: forall a. Seq a -> Tuple a (S.Seq a)
```

O(1). Take one element off the left side of a Seq and return it, together
with the (possibly empty) remainder of the Seq.

#### `unsnoc`

``` purescript
unsnoc :: forall a. Seq a -> Tuple (S.Seq a) a
```

O(1). Take one element off the right side of a Seq and return it, together
with the (possibly empty) remainder of the Seq.

#### `head`

``` purescript
head :: forall a. Seq a -> a
```

O(1). Get the first element of a non-empty sequence. Equivalent to `\seq
-> index seq 0`.

#### `tail`

``` purescript
tail :: forall a. Seq a -> S.Seq a
```

O(1). Get all but the first element of a non-empty sequence. The returned
sequence is possibly empty. Equivalent to `drop 1`.

#### `init`

``` purescript
init :: forall a. Seq a -> S.Seq a
```

O(1). Get all but the last element of a non-empty sequence. Possibly empty.

#### `last`

``` purescript
last :: forall a. Seq a -> a
```

O(1). Get the last element of a non-empty sequence.

#### `toPlain`

``` purescript
toPlain :: forall a. Seq a -> S.Seq a
```

O(1). Turn a non-empty sequence into a "plain" sequence (i.e. one from
Data.Sequence), containing the same elements.

#### `splitAt`

``` purescript
splitAt :: forall a. Number -> Seq a -> Tuple (S.Seq a) (S.Seq a)
```

O(log(min(i,n-i))). Split the sequence into two (possibly empty) subsequences.
The first subsequence will have i elements (unless there are not that many in
the whole sequence, in which case the first element is the same sequence,
unchanged).

#### `take`

``` purescript
take :: forall a. Number -> Seq a -> S.Seq a
```

O(log(min(i,n-i))). Take a certain number of values from the left end of
a sequence, and discard the rest, returning a possibly empty sequence.

#### `drop`

``` purescript
drop :: forall a. Number -> Seq a -> S.Seq a
```

O(log(min(i,n-i))). Discard a given number of elements from the left end
of a sequence, returning a possibly empty sequence.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> Seq a -> S.Seq a
```

O(n). Create a new (possibly empty) sequence which contains only those
elements of the input sequence which satisfy the given predicate.

#### `index`

``` purescript
index :: forall a. Seq a -> Number -> Maybe a
```

O(log(min(i,n-i))). Retrieve the element at the given index in the
sequence. This function is zero-based; that is, the first element in a
sequence `xs` can be retrieved with `index xs 0`.

#### `adjust`

``` purescript
adjust :: forall a. (a -> a) -> Number -> Seq a -> Seq a
```

O(log(min(i,n-i))). Adjust the element at the specified index by
applying the given function to it. If the index is out of range, the
sequence is returned unchanged.

#### `replace`

``` purescript
replace :: forall a. a -> Number -> Seq a -> Seq a
```

O(log(min(i,n-i))). Replace the element at the specified index with
a new element. If the index is out of range, the sequence is returned
unchanged.

#### `fromSeq`

``` purescript
fromSeq :: forall f a. (Functor f, Unfoldable f) => Seq a -> f a
```

Probably O(n), but depends on the Unfoldable instance. Turn a `Seq` into
any `Unfoldable`.

#### `showSeq`

``` purescript
instance showSeq :: (Show a) => Show (Seq a)
```


#### `eqSeq`

``` purescript
instance eqSeq :: (Eq a) => Eq (Seq a)
```


#### `functorSeq`

``` purescript
instance functorSeq :: Functor Seq
```


#### `applySeq`

``` purescript
instance applySeq :: Apply Seq
```


#### `applicativeSeq`

``` purescript
instance applicativeSeq :: Applicative Seq
```


#### `bindSeq`

``` purescript
instance bindSeq :: Bind Seq
```


#### `monadSeq`

``` purescript
instance monadSeq :: Monad Seq
```


#### `semigroupSeq`

``` purescript
instance semigroupSeq :: Semigroup (Seq a)
```


#### `altSeq`

``` purescript
instance altSeq :: Alt Seq
```


#### `foldableSeq`

``` purescript
instance foldableSeq :: Foldable Seq
```


#### `traversableSeq`

``` purescript
instance traversableSeq :: Traversable Seq
```
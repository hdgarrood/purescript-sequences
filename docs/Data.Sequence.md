# Module Documentation

## Module Data.Sequence

#### `semigroupSize`

``` purescript
instance semigroupSize :: Semigroup Size
```


#### `monoidSize`

``` purescript
instance monoidSize :: Monoid Size
```


#### `showSize`

``` purescript
instance showSize :: Show Size
```


#### `measuredElem`

``` purescript
instance measuredElem :: FT.Measured (Elem a) Size
```


#### `showElem`

``` purescript
instance showElem :: (Show a) => Show (Elem a)
```


#### `eqElem`

``` purescript
instance eqElem :: (Eq a) => Eq (Elem a)
```


#### `ordElem`

``` purescript
instance ordElem :: (Ord a) => Ord (Elem a)
```


#### `foldableElem`

``` purescript
instance foldableElem :: Foldable Elem
```


#### `functorElem`

``` purescript
instance functorElem :: Functor Elem
```


#### `traversableElem`

``` purescript
instance traversableElem :: Traversable Elem
```


#### `Seq`

``` purescript
newtype Seq a
```


#### `eqSeq`

``` purescript
instance eqSeq :: (Eq a) => Eq (Seq a)
```


#### `showSeq`

``` purescript
instance showSeq :: (Show a) => Show (Seq a)
```


#### `ordSeq`

``` purescript
instance ordSeq :: (Ord a) => Ord (Seq a)
```


#### `semigroupSeq`

``` purescript
instance semigroupSeq :: Semigroup (Seq a)
```


#### `monoidSeq`

``` purescript
instance monoidSeq :: Monoid (Seq a)
```


#### `foldableSeq`

``` purescript
instance foldableSeq :: Foldable Seq
```


#### `traversableSeq`

``` purescript
instance traversableSeq :: Traversable Seq
```


#### `unfoldableSeq`

``` purescript
instance unfoldableSeq :: Unfoldable Seq
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


#### `altSeq`

``` purescript
instance altSeq :: Alt Seq
```


#### `plusSeq`

``` purescript
instance plusSeq :: Plus Seq
```


#### `alternativeSeq`

``` purescript
instance alternativeSeq :: Alternative Seq
```


#### `monadPlusSeq`

``` purescript
instance monadPlusSeq :: MonadPlus Seq
```


#### `length`

``` purescript
length :: forall a. Seq a -> Number
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
splitAt :: forall a. Number -> Seq a -> Tuple (Seq a) (Seq a)
```

O(log(min(i,n-i))). Split the sequence into two subsequences. The first
subsequence will have i elements (unless there are not that many in the
whole sequence, in which case the first element is the same sequence,
unchanged).

#### `take`

``` purescript
take :: forall a. Number -> Seq a -> Seq a
```

O(log(min(i,n-i))). Discard all elements from a Seq after the first n.

#### `drop`

``` purescript
drop :: forall a. Number -> Seq a -> Seq a
```

O(log(min(i,n-i))). Discard a given number of elements from the left side
of a Seq.

#### `index`

``` purescript
index :: forall a. Seq a -> Number -> Maybe a
```

#### `adjust`

``` purescript
adjust :: forall a. (a -> a) -> Number -> Seq a -> Seq a
```

#### `empty`

``` purescript
empty :: forall a. Seq a
```

A sequence with no elements.

#### `snoc`

``` purescript
snoc :: forall a. Seq a -> a -> Seq a
```

O(1). Add an element to the right end of a Seq.

#### `singleton`

``` purescript
singleton :: forall a. a -> Seq a
```

O(1). Create a Seq with one element.

#### `append`

``` purescript
append :: forall a. Seq a -> Seq a -> Seq a
```

O(log(min(i,n-i))). Join two Seqs together.

#### `head`

``` purescript
head :: forall a. Seq a -> Maybe a
```

O(1). Get the first element of a Seq. Equivalent to `\seq -> index seq 0`.

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
`\seq -> index seq (length seq - 1)`.

#### `toSeq`

``` purescript
toSeq :: forall f a. (Foldable f) => f a -> Seq a
```

#### `fromSeq`

``` purescript
fromSeq :: forall f a. (Functor f, Unfoldable f) => Seq a -> f a
```

Probably O(n), but depends on the Unfoldable instance. Convert a Seq into
some other type, using its Unfoldable instance.
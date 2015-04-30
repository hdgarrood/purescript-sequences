# Module Documentation

## Module Data.Sequence.Ordered


This module defines a sequence where elements are always kept in
order. This enables constant time access to the least and greatest
elements, in addition to logarithmic time partitioning.

The module is intended to be imported qualified, to avoid ambiguity or
name clashes. For example, `import Data.Sequence.Ordered as OS`.

#### `Key`

``` purescript
data Key a
  = NoKey 
  | Key a
```


#### `eqKey`

``` purescript
instance eqKey :: (Eq a) => Eq (Key a)
```


#### `showKey`

``` purescript
instance showKey :: (Show a) => Show (Key a)
```


#### `semigroupKey`

``` purescript
instance semigroupKey :: Semigroup (Key a)
```


#### `ordKey`

``` purescript
instance ordKey :: (Ord a) => Ord (Key a)
```


#### `monoidKey`

``` purescript
instance monoidKey :: Monoid (Key a)
```


#### `measuredElemKey`

``` purescript
instance measuredElemKey :: Measured (Elem a) (Key a)
```


#### `fmapOrdSeq`

``` purescript
fmapOrdSeq :: forall f a. (Functor f) => f (OrdSeqInner a) -> f (OrdSeq a)
```

#### `OrdSeq`

``` purescript
newtype OrdSeq a
  = OrdSeq (OrdSeqInner a)
```

An ordered sequence. The Semigroup instance uses the `merge` function.

#### `OrdSeqInner`

``` purescript
type OrdSeqInner a = FT.FingerTree (Key a) (Elem a)
```


#### `empty`

``` purescript
empty :: forall a. OrdSeq a
```


#### `eqOrdSeq`

``` purescript
instance eqOrdSeq :: (Eq a) => Eq (OrdSeq a)
```


#### `semigroupOrdSeq`

``` purescript
instance semigroupOrdSeq :: (Ord a) => Semigroup (OrdSeq a)
```


#### `monoidOrdSeq`

``` purescript
instance monoidOrdSeq :: (Ord a) => Monoid (OrdSeq a)
```


#### `foldableOrdSeq`

``` purescript
instance foldableOrdSeq :: Foldable OrdSeq
```


#### `null`

``` purescript
null :: forall a. OrdSeq a -> Boolean
```

O(1). Returns true if the given sequence is empty, false otherwise.

#### `length`

``` purescript
length :: forall a. OrdSeq a -> Number
```

O(n). Return the length of the sequence.

#### `partition`

``` purescript
partition :: forall a. (Ord a) => a -> OrdSeq a -> Tuple (OrdSeq a) (OrdSeq a)
```

O(log(n)). Split an ordered sequence into two halves. The first element
of the returned tuple contains all elements which compared less than or
equal to the argument; the second element contains the rest.

#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
```

O(log(n)). Insert the given value into the correct place in the sequence.

#### `deleteAll`

``` purescript
deleteAll :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
```

O(log(n)). Delete all elements from the sequence which compare EQ to the
given value.

#### `merge`

``` purescript
merge :: forall a. (Ord a) => OrdSeq a -> OrdSeq a -> OrdSeq a
```

O(m*log(n/m)), where m and n are the lengths of the longer and shorter
sequences respectively. Create a new sequence containing every element
in both of the given sequences.

#### `least`

``` purescript
least :: forall a. (Ord a) => OrdSeq a -> Maybe a
```

O(1). Access the least element of the sequence, or Nothing if the sequence
is empty.

#### `popLeast`

``` purescript
popLeast :: forall a. (Ord a) => OrdSeq a -> Maybe (Tuple a (OrdSeq a))
```

O(1). Remove the least element of the sequence, returning that element and
the remainder of the sequence. If the sequence is empty, return Nothing.

#### `greatest`

``` purescript
greatest :: forall a. (Ord a) => OrdSeq a -> Maybe a
```

O(1). Access the greatest element of the sequence, or Nothing if the
sequence is empty.

#### `popGreatest`

``` purescript
popGreatest :: forall a. (Ord a) => OrdSeq a -> Maybe (Tuple a (OrdSeq a))
```

O(1). Remove the greatest element of the sequence, returning that element
and the remainder of the sequence. If the sequence is empty, return
Nothing.

#### `toOrdSeq`

``` purescript
toOrdSeq :: forall f a. (Foldable f, Ord a) => f a -> OrdSeq a
```

Probably O(n), but depends on the Foldable instance. Consruct an ordered
sequence from any any `Foldable`.

#### `fromOrdSeq`

``` purescript
fromOrdSeq :: forall f a. (Functor f, Unfoldable f) => OrdSeq a -> f a
```

Probably O(n), but depends on the Unfoldable instance. Unfold an ordered
sequence in ascending order.

#### `fromOrdSeqDescending`

``` purescript
fromOrdSeqDescending :: forall f a. (Functor f, Unfoldable f) => OrdSeq a -> f a
```

Probably O(n), but depends on the Unfoldable instance. Unfold an ordered
sequence in descending order.
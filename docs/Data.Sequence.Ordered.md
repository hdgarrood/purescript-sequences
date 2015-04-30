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


#### `OrdSeq`

``` purescript
newtype OrdSeq a
  = OrdSeq (FT.FingerTree (Key a) (Elem a))
```

An ordered sequence. The Semigroup instance uses the `merge` function.

#### `empty`

``` purescript
empty :: forall a. OrdSeq a
```


#### `semigroupOrdSeq`

``` purescript
instance semigroupOrdSeq :: (Ord a) => Semigroup (OrdSeq a)
```


#### `monoidOrdSeq`

``` purescript
instance monoidOrdSeq :: (Ord a) => Monoid (OrdSeq a)
```


#### `partition`

``` purescript
partition :: forall a. (Ord a) => a -> OrdSeq a -> Tuple (OrdSeq a) (OrdSeq a)
```


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

#### `greatest`

``` purescript
greatest :: forall a. (Ord a) => OrdSeq a -> Maybe a
```

O(1). Access the least element of the sequence, or Nothing if the sequence
is empty.

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
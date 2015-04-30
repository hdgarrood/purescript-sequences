# Module Documentation

## Module Data.Sequence.Ordered


This module defines a sequence where elements are always kept in ascending
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

An ordered sequence.

#### `partition`

``` purescript
partition :: forall a. (Ord a) => a -> OrdSeq a -> Tuple (OrdSeq a) (OrdSeq a)
```


#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
```


#### `deleteAll`

``` purescript
deleteAll :: forall a. (Ord a) => a -> OrdSeq a -> OrdSeq a
```


#### `merge`

``` purescript
merge :: forall a. (Ord a) => OrdSeq a -> OrdSeq a -> OrdSeq a
```
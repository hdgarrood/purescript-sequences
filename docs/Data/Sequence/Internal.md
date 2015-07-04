## Module Data.Sequence.Internal

#### `(!)`

``` purescript
(!) :: forall a. Array a -> Int -> a
```

_left-associative / precedence -1_

#### `(<$$>)`

``` purescript
(<$$>) :: forall f g a b. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
```

_left-associative / precedence -1_

#### `(<$$$>)`

``` purescript
(<$$$>) :: forall f g h a b. (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
```

_left-associative / precedence -1_

#### `strJoin`

``` purescript
strJoin :: forall a. (Show a) => String -> Array a -> String
```

#### `unsafeCoerce`

``` purescript
unsafeCoerce :: forall a b. a -> b
```

#### `Measured`

``` purescript
class Measured a v where
  measure :: a -> v
```

##### Instances
``` purescript
instance measuredArray :: (Monoid v, Measured a v) => Measured (Array a) v
instance measuredLazy :: (Monoid v, Measured a v) => Measured (Lazy a) v
instance measuredElem :: Measured (Elem a) (Additive Int)
instance measuredElemKey :: Measured (Elem a) (Key a)
```

#### `Elem`

``` purescript
newtype Elem a
  = Elem a
```

##### Instances
``` purescript
instance measuredElem :: Measured (Elem a) (Additive Int)
instance showElem :: (Show a) => Show (Elem a)
instance eqElem :: (Eq a) => Eq (Elem a)
instance ordElem :: (Ord a) => Ord (Elem a)
instance foldableElem :: Foldable Elem
instance functorElem :: Functor Elem
instance traversableElem :: Traversable Elem
instance measuredElemKey :: Measured (Elem a) (Key a)
```

#### `getElem`

``` purescript
getElem :: forall a. Elem a -> a
```

#### `mapElem`

``` purescript
mapElem :: forall f a. (Functor f) => f a -> f (Elem a)
```

#### `mapGetElem`

``` purescript
mapGetElem :: forall f a. (Functor f) => f (Elem a) -> f a
```

#### `lift2Elem`

``` purescript
lift2Elem :: forall a b. (b -> a -> b) -> b -> Elem a -> b
```

#### `liftElem`

``` purescript
liftElem :: forall a b. (a -> b) -> Elem a -> b
```

#### `Key`

``` purescript
data Key a
  = NoKey
  | Key a
```

##### Instances
``` purescript
instance eqKey :: (Eq a) => Eq (Key a)
instance showKey :: (Show a) => Show (Key a)
instance semigroupKey :: Semigroup (Key a)
instance ordKey :: (Ord a) => Ord (Key a)
instance monoidKey :: Monoid (Key a)
instance measuredElemKey :: Measured (Elem a) (Key a)
```



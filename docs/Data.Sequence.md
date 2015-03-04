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


#### `null`

``` purescript
null :: forall a. Seq a -> Boolean
```


#### `fromSeq`

``` purescript
fromSeq :: forall f a. (Functor f, Unfoldable f) => Seq a -> f a
```


#### `unconsL`

``` purescript
unconsL :: forall a. Seq a -> Maybe (Tuple a (Seq a))
```


#### `splitAt`

``` purescript
splitAt :: forall a. Number -> Seq a -> Tuple (Seq a) (Seq a)
```


#### `index`

``` purescript
index :: forall a. Seq a -> Number -> a
```


#### `empty`

``` purescript
empty :: forall a. Seq a
```


#### `(<|)`

``` purescript
(<|) :: forall a. a -> Seq a -> Seq a
```


#### `(|>)`

``` purescript
(|>) :: forall a. Seq a -> a -> Seq a
```


#### `singleton`

``` purescript
singleton :: forall a. a -> Seq a
```


#### `append`

``` purescript
append :: forall a. Seq a -> Seq a -> Seq a
```


#### `head`

``` purescript
head :: forall a. Seq a -> Maybe a
```


#### `tail`

``` purescript
tail :: forall a. Seq a -> Maybe (Seq a)
```


#### `init`

``` purescript
init :: forall a. Seq a -> Maybe (Seq a)
```


#### `last`

``` purescript
last :: forall a. Seq a -> Maybe a
```


#### `toSeq`

``` purescript
toSeq :: forall f a. (Foldable f) => f a -> Seq a
```
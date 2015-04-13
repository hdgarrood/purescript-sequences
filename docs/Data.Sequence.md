# Module Documentation

## Module Data.Sequence

#### `Seq`

``` purescript
newtype Seq a
```

##### Instances
``` purescript
instance eqSeq :: (Eq a) => Eq (Seq a)
instance showSeq :: (Show a) => Show (Seq a)
instance ordSeq :: (Ord a) => Ord (Seq a)
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

#### `length`

``` purescript
length :: forall a. (Seq a) -> Number
```


#### `null`

``` purescript
null :: forall a. (Seq a) -> Boolean
```


#### `uncons`

``` purescript
uncons :: forall a. (Seq a) -> Maybe (Tuple a (Seq a))
```


#### `unsnoc`

``` purescript
unsnoc :: forall a. (Seq a) -> Maybe (Tuple (Seq a) a)
```


#### `splitAt`

``` purescript
splitAt :: forall a. Number -> (Seq a) -> Tuple (Seq a) (Seq a)
```


#### `take`

``` purescript
take :: forall a. Number -> (Seq a) -> Seq a
```


#### `drop`

``` purescript
drop :: forall a. Number -> (Seq a) -> Seq a
```


#### `inBounds`

``` purescript
inBounds :: forall a. (Seq a) -> Number -> Boolean
```


#### `index`

``` purescript
index :: forall a. (Seq a) -> Number -> Maybe a
```


#### `adjust`

``` purescript
adjust :: forall a. (a -> a) -> Number -> (Seq a) -> Seq a
```


#### `replace`

``` purescript
replace :: forall a. a -> Number -> (Seq a) -> Seq a
```


#### `empty`

``` purescript
empty :: forall a. Seq a
```


#### `cons`

``` purescript
cons :: forall a. a -> (Seq a) -> Seq a
```


#### `snoc`

``` purescript
snoc :: forall a. (Seq a) -> a -> Seq a
```


#### `singleton`

``` purescript
singleton :: forall a. a -> Seq a
```


#### `append`

``` purescript
append :: forall a. (Seq a) -> (Seq a) -> Seq a
```


#### `head`

``` purescript
head :: forall a. (Seq a) -> Maybe a
```


#### `tail`

``` purescript
tail :: forall a. (Seq a) -> Maybe (Seq a)
```


#### `init`

``` purescript
init :: forall a. (Seq a) -> Maybe (Seq a)
```


#### `last`

``` purescript
last :: forall a. (Seq a) -> Maybe a
```


#### `toSeq`

``` purescript
toSeq :: forall f a. (Foldable f) => (f a) -> Seq a
```


#### `fromSeq`

``` purescript
fromSeq :: forall f a. (Functor f, Unfoldable f) => (Seq a) -> f a
```


#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> (Seq a) -> Seq a
```





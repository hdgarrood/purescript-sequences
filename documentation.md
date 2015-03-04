# Module Documentation

## Module Data.FingerTree

#### `toArray`

``` purescript
toArray :: forall f a. (Foldable f) => f a -> [a]
```

#### `Measured`

``` purescript
class Measured a v where
  measure :: a -> v
```


#### `Node`

``` purescript
data Node v a
  = Node2 v a a
  | Node3 v a a a
```


#### `showNode`

``` purescript
instance showNode :: (Show a, Show v) => Show (Node v a)
```


#### `node2`

``` purescript
node2 :: forall a v. (Monoid v, Measured a v) => a -> a -> Node v a
```


#### `node3`

``` purescript
node3 :: forall a v. (Monoid v, Measured a v) => a -> a -> a -> Node v a
```


#### `functorNode`

``` purescript
instance functorNode :: Functor (Node v)
```


#### `foldableNode`

``` purescript
instance foldableNode :: Foldable (Node v)
```


#### `traversableNode`

``` purescript
instance traversableNode :: Traversable (Node v)
```


#### `measuredNode`

``` purescript
instance measuredNode :: Measured (Node v a) v
```


#### `measuredArray`

``` purescript
instance measuredArray :: (Monoid v, Measured a v) => Measured [a] v
```


#### `measuredLazy`

``` purescript
instance measuredLazy :: (Monoid v, Measured a v) => Measured (Lazy a) v
```


#### `FingerTree`

``` purescript
data FingerTree v a
  = Empty 
  | Single a
  | Deep (Lazy v) (Digit a) (Lazy (FingerTree v (Node v a))) (Digit a)
```

#### `lazyEmpty`

``` purescript
lazyEmpty :: forall v a. Lazy (FingerTree v a)
```


#### `deep`

``` purescript
deep :: forall a v. (Monoid v, Measured a v) => Digit a -> Lazy (FingerTree v (Node v a)) -> Digit a -> FingerTree v a
```


#### `Digit`

``` purescript
type Digit a = [a]
```

#### `showFingerTree`

``` purescript
instance showFingerTree :: (Show v, Show a) => Show (FingerTree v a)
```


#### `compareFingerTree`

``` purescript
compareFingerTree :: forall a v. (Monoid v, Measured a v, Ord a) => FingerTree v a -> FingerTree v a -> Ordering
```

#### `(<$$>)`

``` purescript
(<$$>) :: forall f g a b. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
```


#### `(<$$$>)`

``` purescript
(<$$$>) :: forall f g h a b. (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
```


#### `functorFingerTree`

``` purescript
instance functorFingerTree :: Functor (FingerTree v)
```


#### `foldableFingerTree`

``` purescript
instance foldableFingerTree :: Foldable (FingerTree v)
```


#### `traversableFingerTree`

``` purescript
instance traversableFingerTree :: Traversable (FingerTree v)
```


#### `measuredFingerTree`

``` purescript
instance measuredFingerTree :: (Monoid v, Measured a v) => Measured (FingerTree v a) v
```


#### `(<|)`

``` purescript
(<|) :: forall a v. (Monoid v, Measured a v) => a -> FingerTree v a -> FingerTree v a
```


#### `(|>)`

``` purescript
(|>) :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> a -> FingerTree v a
```


#### `(<<|)`

``` purescript
(<<|) :: forall f a v. (Monoid v, Measured a v, Foldable f) => f a -> FingerTree v a -> FingerTree v a
```


#### `(|>>)`

``` purescript
(|>>) :: forall f a v. (Monoid v, Measured a v, Foldable f) => FingerTree v a -> f a -> FingerTree v a
```


#### `toTree`

``` purescript
toTree :: forall f a v. (Monoid v, Measured a v, Foldable f) => f a -> FingerTree v a
```


#### `ViewL`

``` purescript
data ViewL s a
  = NilL 
  | ConsL a (Lazy (s a))
```


#### `functorViewL`

``` purescript
instance functorViewL :: (Functor s) => Functor (ViewL s)
```


#### `switchViewL`

``` purescript
switchViewL :: forall s t a b. (a -> b) -> (s a -> t b) -> ViewL s a -> ViewL t b
```


#### `headDigit`

``` purescript
headDigit :: forall a. Digit a -> a
```


#### `tailDigit`

``` purescript
tailDigit :: forall a. Digit a -> Digit a
```


#### `viewL`

``` purescript
viewL :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> ViewL (FingerTree v) a
```


#### `deepL`

``` purescript
deepL :: forall a v. (Monoid v, Measured a v) => [a] -> Lazy (FingerTree v (Node v a)) -> [a] -> FingerTree v a
```


#### `isEmpty`

``` purescript
isEmpty :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Boolean
```


#### `headL`

``` purescript
headL :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Maybe a
```


#### `tailL`

``` purescript
tailL :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Maybe (FingerTree v a)
```


#### `lastDigit`

``` purescript
lastDigit :: forall a. Digit a -> a
```


#### `initDigit`

``` purescript
initDigit :: forall a. Digit a -> Digit a
```


#### `ViewR`

``` purescript
data ViewR s a
  = NilR 
  | SnocR (Lazy (s a)) a
```


#### `viewR`

``` purescript
viewR :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> ViewR (FingerTree v) a
```


#### `deepR`

``` purescript
deepR :: forall a v. (Monoid v, Measured a v) => [a] -> Lazy (FingerTree v (Node v a)) -> [a] -> FingerTree v a
```


#### `headR`

``` purescript
headR :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Maybe a
```


#### `tailR`

``` purescript
tailR :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Maybe (FingerTree v a)
```


#### `app3`

``` purescript
app3 :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> [a] -> FingerTree v a -> FingerTree v a
```


#### `nodes`

``` purescript
nodes :: forall a v. (Monoid v, Measured a v) => [a] -> [Node v a]
```


#### `append`

``` purescript
append :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> FingerTree v a -> FingerTree v a
```


#### `Split`

``` purescript
data Split f a
  = Split (f a) a (f a)
```


#### `LazySplit`

``` purescript
data LazySplit f a
  = LazySplit (Lazy (f a)) a (Lazy (f a))
```


#### `splitDigit`

``` purescript
splitDigit :: forall a v. (Monoid v, Measured a v) => (v -> Boolean) -> v -> Digit a -> Split Prim.Array a
```

#### `splitTree`

``` purescript
splitTree :: forall a v. (Monoid v, Measured a v) => (v -> Boolean) -> v -> FingerTree v a -> LazySplit (FingerTree v) a
```

#### `split`

``` purescript
split :: forall a v. (Monoid v, Measured a v) => (v -> Boolean) -> FingerTree v a -> Tuple (Lazy (FingerTree v a)) (Lazy (FingerTree v a))
```



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


#### `toArray`

``` purescript
toArray :: forall a. Seq a -> [a]
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


#### `headL`

``` purescript
headL :: forall a. Seq a -> Maybe a
```


#### `tailL`

``` purescript
tailL :: forall a. Seq a -> Maybe (Seq a)
```


#### `toSeq`

``` purescript
toSeq :: forall f a. (Foldable f) => f a -> Seq a
```
# Module Documentation

## Module Data.FingerTree

#### `Reduce`

``` purescript
class Reduce f where
  reducer :: forall a b. (a -> b -> b) -> f a -> b -> b
  reducel :: forall a b. (b -> a -> b) -> b -> f a -> b
```


#### `arrayReduce`

``` purescript
instance arrayReduce :: Reduce Prim.Array
```

#### `toArray`

``` purescript
toArray :: forall f a. (Reduce f) => f a -> [a]
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


#### `reduceNode`

``` purescript
instance reduceNode :: Reduce (Node v)
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


#### `reduceFingerTree`

``` purescript
instance reduceFingerTree :: Reduce (FingerTree v)
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
(<<|) :: forall f a v. (Monoid v, Measured a v, Reduce f) => f a -> FingerTree v a -> FingerTree v a
```


#### `(|>>)`

``` purescript
(|>>) :: forall f a v. (Monoid v, Measured a v, Reduce f) => FingerTree v a -> f a -> FingerTree v a
```


#### `toTree`

``` purescript
toTree :: forall f a v. (Monoid v, Measured a v, Reduce f) => f a -> FingerTree v a
```


#### `ViewL`

``` purescript
data ViewL s a
  = NilL 
  | ConsL a (Lazy (s a))
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


#### `(><)`

``` purescript
(><) :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> FingerTree v a -> FingerTree v a
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



## Module Sequence

#### `Size`

``` purescript
newtype Size
  = Size Number
```


#### `getSize`

``` purescript
getSize :: Size -> Number
```


#### `semigrouSize`

``` purescript
instance semigrouSize :: Semigroup Size
```


#### `monoidSize`

``` purescript
instance monoidSize :: Monoid Size
```


#### `showSize`

``` purescript
instance showSize :: Show Size
```


#### `Elem`

``` purescript
newtype Elem a
  = Elem a
```


#### `getElem`

``` purescript
getElem :: forall a. Elem a -> a
```


#### `measuredElem`

``` purescript
instance measuredElem :: FT.Measured (Elem a) Size
```


#### `showElem`

``` purescript
instance showElem :: (Show a) => Show (Elem a)
```


#### `Seq`

``` purescript
type Seq a = FT.FingerTree Size (Elem a)
```


#### `length`

``` purescript
length :: forall a. Seq a -> Number
```


#### `splitAt`

``` purescript
splitAt :: forall a. Number -> Seq a -> Tuple (Lazy (Seq a)) (Lazy (Seq a))
```


#### `(!)`

``` purescript
(!) :: forall a. Seq a -> Number -> a
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


#### `(><)`

``` purescript
(><) :: forall a. Seq a -> Seq a -> Seq a
```


#### `headL`

``` purescript
headL :: forall a. Seq a -> Maybe a
```


#### `tailL`

``` purescript
tailL :: forall a. Seq a -> Maybe (Seq a)
```
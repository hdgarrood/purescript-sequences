module Data.FingerTree where

-- An implementation of finger trees, based on "Finger Trees: A Simple,
-- General-Purpose Data Structure" (2006), Ralf Hinze and Ross Paterson.
-- http://staff.city.ac.uk/~ross/papers/FingerTree.pdf

import Data.Monoid
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Maybe
import Data.Tuple
import Data.Lazy
import Data.Foldable
import Data.Unfoldable
import Data.Traversable

fromFingerTree :: forall f a v. (Unfoldable f, Monoid v, Measured a v) =>
  FingerTree v a -> f a
fromFingerTree = unfoldr step
  where
  step tree = case viewL tree of
                ConsL x xs -> Just (Tuple x (force xs))
                NilL       -> Nothing

class Measured a v where
  measure :: a -> v

data Node v a = Node2 v a a | Node3 v a a a

instance showNode :: (Show a, Show v) => Show (Node v a) where
  show (Node2 v a b) =
    ("Node2 (" ++ show v
     ++ ") (" ++ show a
     ++ ") (" ++ show b
     ++ ")")
  show (Node3 v a b c) =
    ("Node3 (" ++ show v
     ++ ") (" ++ show a
     ++ ") (" ++ show b
     ++ ") (" ++ show c
     ++ ")")

node2 :: forall a v. (Monoid v, Measured a v) => a -> a -> Node v a
node2 a b = Node2 (measure a <> measure b) a b

node3 :: forall a v. (Monoid v, Measured a v) => a -> a -> a -> Node v a
node3 a b c = Node3 (measure a <> measure b <> measure c) a b c

nodeToDigit :: forall a v. Node v a -> Digit a
nodeToDigit (Node2 _ a b) = [a, b]
nodeToDigit (Node3 _ a b c) = [a, b, c]

instance functorNode :: Functor (Node v) where
  (<$>) f (Node2 v a b)   = Node2 v (f a) (f b)
  (<$>) f (Node3 v a b c) = Node3 v (f a) (f b) (f c)

instance foldableNode :: Foldable (Node v) where
  foldr (-<) z (Node2 _ a b)   = a -< (b -< z)
  foldr (-<) z (Node3 _ a b c) = a -< (b -< (c -< z))
  foldl (>-) z (Node2 _ a b)   = (z >- a) >- b
  foldl (>-) z (Node3 _ a b c) = ((z >- a) >- b) >- c
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

instance traversableNode :: Traversable (Node v) where
  traverse f (Node2 v a b) = Node2 v <$> f a <*> f b
  traverse f (Node3 v a b c) = Node3 v <$> f a <*> f b <*> f c
  sequence = traverse id

instance measuredNode :: Measured (Node v a) v where
  measure (Node2 v _ _) = v
  measure (Node3 v _ _ _) = v

instance measuredArray :: (Monoid v, Measured a v) => Measured [a] v where
  measure xs = foldl (\i a -> i <> measure a) mempty xs

instance measuredLazy :: (Monoid v, Measured a v) => Measured (Lazy a) v where
  measure s = measure (force s)

-- Deep node may have debits, the cost of suspended code, as many as safe
-- digits it has (i.e., 0, 1, or 2).
data FingerTree v a = Empty
                    | Single a
                    | Deep
                      (Lazy v)
                      (Digit a)
                      (Lazy (FingerTree v (Node v a)))
                      (Digit a)


lazyEmpty :: forall v a. Lazy (FingerTree v a)
lazyEmpty = defer (\_ -> Empty)

deep :: forall a v. (Monoid v, Measured a v)
     => Digit a
     -> Lazy (FingerTree v (Node v a))
     -> Digit a
     -> FingerTree v a
deep pr m sf =
  Deep (defer (\_ -> measure pr <> measure m <> measure sf)) pr m sf

-- Digit has one to four elements.
-- If Digit has two or three elements, it is safe; otherwise it is dangerous.
type Digit a = [a]

instance showFingerTree :: (Show v, Show a) => Show (FingerTree v a) where
  show Empty = "Empty"
  show (Single a) = "Single (" ++ show a ++ ")"
  show (Deep v pr m sf) =
    ("Deep (" ++ show v
     ++ ") (" ++ show pr
     ++ ") (" ++ show m
     ++ ") (" ++ show sf
     ++ ")")

-- We don't implement an Ord instance because we can't implement a good Eq
-- instance, and because we expect actual uses of FingerTrees to use newtypes,
-- so we provide this function instead to help with defining Ord instances.
compareFingerTree :: forall a v. (Monoid v, Measured a v, Ord a) =>
  FingerTree v a -> FingerTree v a -> Ordering
compareFingerTree xs ys =
  case Tuple (viewL xs) (viewL ys) of
    Tuple NilL NilL -> EQ
    Tuple NilL _    -> LT
    Tuple _    NilL -> GT
    Tuple (ConsL x xs') (ConsL y ys') ->
      case compare x y of
        EQ -> let xs'' = force xs'
                  ys'' = force ys'
              in compareFingerTree xs'' ys''
        other -> other

(<$$>) :: forall f g a b. (Functor f, Functor g) =>
  (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) <<< (<$>)

(<$$$>) :: forall f g h a b. (Functor f, Functor g, Functor h) =>
  (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = (<$$>) <<< (<$>)

instance functorFingerTree :: Functor (FingerTree v) where
  (<$>) f Empty = Empty
  (<$>) f (Single x) = Single (f x)
  (<$>) f (Deep v pr m sf) = Deep v (f <$> pr) (f <$$$> m) (f <$> sf)

instance foldableFingerTree :: Foldable (FingerTree v) where
  foldr (-<) z Empty            = z
  foldr (-<) z (Single x)       = x -< z
  foldr (-<) z (Deep _ pr m sf) = pr +<< ((force m) -<<< (sf -<< z))
    where
    (-<<) = flip (foldr (-<))
    -- this is a hack to get type inference to work
    (+<<) = flip (foldr (-<))
    (-<<<) = flip (foldr (flip (foldr (-<))))


  foldl (>-) z Empty            = z
  foldl (>-) z (Single x)       = z >- x
  foldl (>-) z (Deep _ pr m sf) = ((z >>- pr) >>>- (force m)) >>- sf
    where
    (>>-) = foldl (>-)
    (>>>-) = foldl (foldl (>-))

  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs


instance traversableFingerTree :: Traversable (FingerTree v) where
  traverse f Empty            = pure Empty
  traverse f (Single x)       = Single <$> f x
  traverse f (Deep v pr m sf) =
    Deep v <$> traverse f pr
           <*> (defer <$> kl)
           <*> traverse f sf
    where
    l :: m (FingerTree v (Node v a))
    l = traverse (traverse f) (force m)

    kl :: m (Unit -> FingerTree v (Node v a))
    kl = const <$> l

  sequence = traverse id

instance measuredFingerTree :: (Monoid v, Measured a v)
                            => Measured (FingerTree v a) v where
  measure Empty = mempty
  measure (Single x) = measure x
  measure (Deep v _ _ _) = (force v)

infixr 5 <|

(<|) :: forall a v. (Monoid v, Measured a v) =>
  a -> FingerTree v a -> FingerTree v a
(<|) a Empty                      = Single a
(<|) a (Single b)                 = deep [a] lazyEmpty [b]
(<|) a (Deep _ [b, c, d, e] m sf) =
  let
    -- If sf is safe, we pass one debit to the outer suspension to force the
    -- suspension. If sf is dangerous, we have no debits, so that we can
    -- freely force the suspension.
    forcedM = force m
  in
   -- Since we turn a dangerous digit to safe digit, we will get one extra
   -- debit allowance after prepend. We creates one debit for unshared cost
   -- of recursive call. We receives another debit from recursive call.
   -- If sf is safe, we now have two debit allowance, so that the constraint
   -- is satisfied. if sf is dangerous, we can pass a debit to the outer
   -- suspension to satisfy constraint.
   deep [a, b] (defer (\_ -> node3 c d e <| forcedM)) sf
(<|) a (Deep _ pr m sf)           = deep (a : pr) m sf

infixl 5 |>

(|>) :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> a -> FingerTree v a
(|>) Empty                      a = Single a
(|>) (Single b)                 a = deep [b] lazyEmpty [a]
(|>) (Deep _ pr m [e, d, c, b]) a =
  let
    forcedM = force m
  in
   deep pr (defer (\_ -> forcedM |> node3 e d c)) [b, a]
(|>) (Deep _ pr m sf)           a = deep pr m (A.snoc sf a)

(<<|) :: forall f a v. (Monoid v, Measured a v, Foldable f)
      => f a -> FingerTree v a -> FingerTree v a
(<<|) = flip (foldr (<|))

(|>>) :: forall f a v. (Monoid v, Measured a v, Foldable f)
      => FingerTree v a -> f a -> FingerTree v a
(|>>) = foldl (|>)

toFingerTree :: forall f a v. (Monoid v, Measured a v, Foldable f) =>
  f a -> FingerTree v a
toFingerTree s = s <<| Empty

data ViewL s a = NilL | ConsL a (Lazy (s a))

instance functorViewL :: (Functor s) => Functor (ViewL s) where
  (<$>) f NilL = NilL
  (<$>) f (ConsL x xs) = ConsL (f x) ((f <$>) <$> xs)

headDigit :: forall a. Digit a -> a
headDigit = AU.head

tailDigit :: forall a. Digit a -> Digit a
tailDigit = AU.tail

viewL :: forall a v. (Monoid v, Measured a v)
      => FingerTree v a -> ViewL (FingerTree v) a
viewL Empty            = NilL
viewL (Single   x)     = ConsL x lazyEmpty
-- If pr has more than two elements, no debits are discharged.
-- If pr has exactly two elements, debit allowance is decreased  by one,
-- so that passes it to the outer suspension.
-- If pr has exactly one element, we further analyse sf:
-- - If sf is safe, we pass a debit to the outer suspension to force
--   the suspension. One debit is created for the unshared cost of the
--   recursive call. We receive another debit from recursive call.
--   We now have two debits and two debit allowance
--   (note that both digits are now safe), the constraint is satisfied.
-- - If sf is dangerous, the debits of the node is zero, so that we can
--   force the suspension for free. One debit is created for the unshared
--   cost of the recursive call. We receive another debit from recursive
--   call. We now have two debit and one debit allowance, so that passing
--   one debit to outer suspension satisfies the constraint.
viewL (Deep _ pr m sf) =
  ConsL (headDigit pr) (defer (\_ -> deepL (tailDigit pr) m sf))

deepL :: forall a v. (Monoid v, Measured a v)
      => [a] -> Lazy (FingerTree v (Node v a)) -> [a] -> FingerTree v a
deepL [] m sf = case viewL (force m) of
  NilL       -> toFingerTree sf
  ConsL a m' -> deep (nodeToDigit a) m' sf
deepL pr m sf = deep pr m sf

isEmpty :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Boolean
isEmpty x = case viewL x of
  NilL      -> true
  ConsL _ _ -> false

head :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Maybe a
head x = case viewL x of
  ConsL a _ -> Just a
  NilL      -> Nothing

tail :: forall a v. (Monoid v, Measured a v) =>
  FingerTree v a -> Maybe (FingerTree v a)
tail x = case viewL x of
  ConsL _ x' -> Just (force x')
  NilL       -> Nothing

lastDigit :: forall a. Digit a -> a
lastDigit = AU.last

initDigit :: forall a. Digit a -> Digit a
initDigit = AU.init

data ViewR s a = NilR | SnocR (Lazy (s a)) a

viewR :: forall a v. (Monoid v, Measured a v)
      => FingerTree v a -> ViewR (FingerTree v) a
viewR Empty            = NilR
viewR (Single x)       = SnocR lazyEmpty x
viewR (Deep _ pr m sf) =
  SnocR (defer (\_ -> deepR pr m (initDigit sf))) (lastDigit sf)

deepR :: forall a v. (Monoid v, Measured a v)
      => [a] -> Lazy (FingerTree v (Node v a)) -> [a] -> FingerTree v a
deepR pr m [] = case viewR (force m) of
  NilR       -> toFingerTree pr
  SnocR m' a -> deep pr m' (nodeToDigit a)
deepR pr m sf = deep pr m sf

last :: forall a v. (Monoid v, Measured a v) => FingerTree v a -> Maybe a
last x = case viewR x of
  SnocR _ a -> Just a
  NilR      -> Nothing

init :: forall a v. (Monoid v, Measured a v) =>
  FingerTree v a -> Maybe (FingerTree v a)
init x = case viewR x of
  SnocR x' _ -> Just (force x')
  NilR       -> Nothing

app3 :: forall a v. (Monoid v, Measured a v)
     => FingerTree v a -> [a] -> FingerTree v a -> FingerTree v a
app3 Empty ts xs      = ts <<| xs
app3 xs ts Empty      = xs |>> ts
app3 (Single x) ts xs = x <| (ts <<| xs)
app3 xs ts (Single x) = (xs |>> ts) |> x
app3 (Deep _ pr1 m1 sf1) ts (Deep _ pr2 m2 sf2) =
  let
    computeM' _ =
      app3 (force m1) (nodes (sf1 <> ts <> pr2)) (force m2)
  in
   deep pr1 (defer computeM') sf2

-- TODO: potential performance issues here?
nodes :: forall a v. (Monoid v, Measured a v) => [a] -> [Node v a]
nodes [a, b]           = [node2 a b]
nodes [a, b, c]        = [node3 a b c]
nodes [a, b, c, d]     = [node2 a b, node2 c d]
nodes (a : b : c : xs) = node3 a b c : nodes xs

append :: forall a v. (Monoid v, Measured a v)
     => FingerTree v a -> FingerTree v a -> FingerTree v a
append xs ys = app3 xs [] ys

data Split f a = Split (f a) a (f a)
data LazySplit f a = LazySplit (Lazy (f a)) a (Lazy (f a))

-- unsafe
splitDigit :: forall a v. (Monoid v, Measured a v)
           => (v -> Boolean) -> v -> Digit a -> Split [] a
splitDigit p i [a] = Split [] a []
splitDigit p i (a:as) =
  if p i'
    then Split [] a as
    else case splitDigit p i' as of
              Split l x r -> Split (a:l) x r
  where
  i' = i <> measure a

-- unsafe
splitTree :: forall a v. (Monoid v, Measured a v)
          => (v -> Boolean) -> v -> FingerTree v a -> LazySplit (FingerTree v) a
splitTree p i (Single x) = LazySplit lazyEmpty x lazyEmpty
splitTree p i (Deep _ pr m sf) =
  let vpr = i <> measure pr
  in if p vpr
    then case splitDigit p i pr of
      Split l x r ->
        LazySplit (defer (\_ -> toFingerTree l)) x (defer (\_ -> deepL r m sf))
    else
      let vm = vpr <> measure m
      in if p vm
        then
          case splitTree p vpr (force m) of
            LazySplit ml xs mr ->
              case splitDigit p (vpr <> measure ml) (nodeToDigit xs) of
                Split l x r ->
                  LazySplit (defer (\_ -> deepR pr ml l))
                            x
                            (defer (\_ -> deepL r mr sf))
        else
          case splitDigit p vm sf of
           Split l x r ->
             LazySplit (defer (\_ -> deepR pr m l))
                       x
                       (defer (\_ -> toFingerTree r))

split :: forall a v. (Monoid v, Measured a v)
      => (v -> Boolean)
      -> FingerTree v a
      -> Tuple (Lazy (FingerTree v a)) (Lazy (FingerTree v a))
split p Empty = Tuple lazyEmpty lazyEmpty
split p xs =
  if p (measure xs)
  then
    case splitTree p mempty xs of
      LazySplit l x r ->
        Tuple l (defer (\_ -> (x <| (force r))))
  else
    Tuple (defer (\_ -> xs)) lazyEmpty

module Tests.Data.Sequence (sequenceTests) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as A
import Data.Foldable (all, foldl, foldr, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Sequence (index)
import Data.Sequence as S
import Data.Tuple (Tuple(..), fst, snd)
import Test.QuickCheck ((<?>), (===), quickCheck)
import Test.QuickCheck.Laws (A)
import Test.QuickCheck.Laws.Control.Alt (checkAlt)
import Test.QuickCheck.Laws.Control.Alternative (checkAlternative)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.MonadPlus (checkMonadPlus)
import Test.QuickCheck.Laws.Control.Plus (checkPlus)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Tests.Utils (ArbSeq(ArbSeq), err, abs, integerBetween, foldableSize)
import Type.Proxy (Proxy(Proxy), Proxy2(Proxy2))

arr :: forall a. S.Seq a -> Array a
arr = S.toUnfoldable

prx :: Proxy (ArbSeq A)
prx = Proxy

prx2 :: Proxy2 ArbSeq
prx2 = Proxy2

checkElemIndex :: forall a. Eq a => S.Seq a -> a -> Boolean
checkElemIndex seq x =
    case S.elemIndex x seq of
    Nothing -> S.null (S.filter (eq x) seq)
    Just i ->
      case S.index i seq of
      Nothing -> false
      Just e -> e == x && S.null (S.filter (eq x) (S.take i seq))

sequenceTests :: forall t.
        Eff
          ( console :: CONSOLE
          , random :: RANDOM
          , exception :: EXCEPTION
          | t
          )
          Unit
sequenceTests = do
  log ""
  log "Data.Sequence"
  log "============="
  log ""

  checkEq prx
  checkOrd prx
  checkFunctor prx2
  checkApply prx2
  checkApplicative prx2
  checkBind prx2
  checkMonad prx2
  checkSemigroup prx
  checkMonoid prx
  checkAlt prx2
  checkPlus prx2
  checkAlternative prx2
  checkMonadPlus prx2

  log "Test toUnfoldable homomorphism"
  quickCheck $ \(ArbSeq x) (ArbSeq y) ->
    arr (x <> y) == arr x <> (arr y :: Array Number)
    <?> ("x: " <> show x <> ", y: " <> show y)

  log "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z (S.fromFoldable xs) == foldr f z (xs :: Array Number)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z (S.fromFoldable xs) == foldl f z (xs :: Array Number)

  quickCheck $ \xs -> A.length xs == foldableSize (S.fromFoldable xs :: S.Seq Number)
  quickCheck $ \(ArbSeq xs) -> A.length (arr xs) == foldableSize (xs :: S.Seq Number)

  log "Test length/null"
  quickCheck $ \(ArbSeq xs) ->
    if S.empty == (xs :: S.Seq Number) then S.null xs else S.length xs > 0

  quickCheck $ \(ArbSeq xs) -> S.length xs + 1 == S.length (S.cons 0 xs)
  quickCheck $ \(ArbSeq xs) ->
    let xs' = S.cons 0 xs -- ensure xs' has at least one element
    in S.length xs' - 1 == S.length (S.drop 1 xs')

  log "Test splitAt/head/last"
  quickCheck $ \idx (ArbSeq seq) ->
    let idx' :: Int
        idx' = integerBetween 0 (S.length seq) idx

        split :: Tuple (S.Seq Number) (S.Seq Number)
        split = S.splitAt idx' seq

    in  S.last (fst split) == S.index (idx' - 1) seq
          && S.head (snd split) == S.index idx' seq
          <?> ("seq: " <> show seq <> ", idx':" <> show idx')

  log "Test that adjust is safe"
  quickCheck $ \(ArbSeq seq) ->
    let f n = S.adjust id n (seq :: S.Seq Number)
    in f (-1) == f (S.length seq)

  log "Test that index is safe"
  quickCheck $ \(ArbSeq seq) ->
    let f n = S.index n (seq :: S.Seq Number)
    in f (-1) == Nothing && f (S.length seq) == Nothing

  log "Test inBounds"
  quickCheck $ \(ArbSeq seq) ->
    let seq' = S.cons 0 seq
        lowerBound = 0
        upperBound = S.length seq' - 1
    in S.inBounds lowerBound seq' && S.inBounds upperBound seq'
        && not (S.inBounds (lowerBound - 1) seq')
        && not (S.inBounds (upperBound + 1) seq')

  log "Test adjust"
  quickCheck $ \(ArbSeq seq) idx ->
    let seq' = const 0 <$> S.cons 0 seq
        idx' = integerBetween 0 (S.length seq') idx
        result = sum (S.adjust (_+1) idx' seq')
    in  result == 1 <?> "seq': " <> show seq' <> ", result: " <> show result

  log "Test adjust modifies at the correct index"
  quickCheck $ \(ArbSeq seq) idx ->
    let seq' = const "hello" <$> S.cons 0 seq
        idx' = integerBetween 0 (S.length seq') idx
        result = S.index idx' (S.adjust (_ <> ", world") idx' seq')
    in  (result == Just "hello, world")
          <?> "seq': " <> show seq' <> ", result: " <> show result

  log "Test take"
  -- We must account for potentially negative indices generated by QuickCheck
  -- take is defined over negative indices, though the length of the result
  -- will be 0, so we must check that the result length is less than the
  -- absolute value of the index
  quickCheck $ \(ArbSeq seq) n ->
    let result = S.length $ S.take n $ seq :: S.Seq Number
    in (0 <= result && result <= abs n) <?> err [ "n = " <> show n
                                                , "seq = " <> show seq
                                                , "result = " <> show result
                                                ]

  log "Test drop"
  -- See note on "Test take" about negative indices
  quickCheck $ \(ArbSeq seq) n ->
    let dropped = S.length (seq :: S.Seq Number) - S.length (S.drop n seq)
    in (0 <= dropped && dropped <= abs n) <?> err [ "n = " <> show n
                                                  , "seq = " <> show seq
                                                  , "dropped = " <> show dropped
                                                  ]

  log "Test filter"
  quickCheck $ \(ArbSeq seq) -> S.null (S.filter (const false) (seq :: S.Seq Number))
  quickCheck $ \(ArbSeq seq) -> S.filter (const true) seq === (seq :: S.Seq Number)
  quickCheck $ \(ArbSeq seq) f -> all f (S.filter f (seq :: S.Seq Number))

  log "Test cons/uncons"
  quickCheck $ \(ArbSeq seq) x ->
    S.uncons (S.cons x seq) === Just (Tuple (x :: Number) seq)
  quickCheck $ \(ArbSeq seq) x ->
    S.unsnoc (S.snoc seq x) === Just (Tuple seq (x :: Number))

  log "Test init"
  quickCheck $ \(ArbSeq seq) ->
    fromMaybe S.empty (S.init seq) === S.take (S.length seq - 1) (seq :: S.Seq Number)

  log "Test tail"
  quickCheck $ \(ArbSeq seq) ->
    fromMaybe S.empty (S.tail seq) === S.drop 1 (seq :: S.Seq Number)

  log "Test head"
  quickCheck $ \(ArbSeq seq) x ->
    S.head (S.cons x seq) === Just (x :: Number)

  log "Test last"
  quickCheck $ \(ArbSeq seq) x ->
    S.last (S.snoc seq x) === Just (x :: Number)

  log "Test elemIndex"
  quickCheck $ \(ArbSeq seq) x ->
    checkElemIndex seq (x :: Number)
  quickCheck $ \(ArbSeq left) x (ArbSeq right) ->
    checkElemIndex (S.append left (S.cons x right)) (x :: Number)

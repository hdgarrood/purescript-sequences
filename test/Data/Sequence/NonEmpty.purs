module Tests.Data.Sequence.NonEmpty (nonEmptySequenceTests) where

import Prelude

import           Control.Monad.Eff.Console (log)
import qualified Data.Array                as A
import           Data.Foldable             (all, foldl, foldr, sum)
import           Data.Maybe                (Maybe(Nothing))
import           Test.QuickCheck           ((<?>), (===), quickCheck)
import           Data.Tuple                (Tuple(Tuple), fst, snd)

import qualified Data.Sequence          as S
import qualified Data.Sequence.NonEmpty as NonEmpty
import           Tests.Utils            ( abs
                                        , err
                                        , foldableSize
                                        , integerBetween
                                        )
import           TypeClassTests         ( checkApplicative
                                        , checkFunctor
                                        , checkMonad
                                        )

nonEmptySequenceTests = do
  log ""
  log "Data.Sequence.NonEmpty"
  log "======================"
  log ""

  log "Test fromSeq homomorphism"
  quickCheck $ \x y ->
    NonEmpty.fromSeq (x <> y) == NonEmpty.fromSeq x <> (NonEmpty.fromSeq y :: Array Number)
    <?> ("x: " <> show x <> ", y: " <> show y)

  log "Test semigroup law: associativity"
  quickCheck $ \x y z -> (x <> y) <> z == x <> (y <> z :: NonEmpty.Seq Number)
    <?> ("x: " <> show x <> ", y: " <> show y <> ", z:" <> show z)

  let proxy = NonEmpty.singleton 0
  log "Test functor laws"
  checkFunctor proxy

  log "Test applicative laws"
  checkApplicative proxy proxy proxy

  log "Test monad laws"
  checkMonad proxy

  log "Test foldable instance"
  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldr f z xs == foldr f z (NonEmpty.fromSeq xs :: Array Number)

  quickCheck $ \f z xs ->
    let types = Tuple (f :: Number -> Number -> Number) (z :: Number)
    in  foldl f z xs == foldl f z (NonEmpty.fromSeq xs :: Array Number)

  quickCheck $ \xs -> A.length (NonEmpty.fromSeq xs) == foldableSize (xs :: NonEmpty.Seq Number)

  quickCheck $ \xs -> NonEmpty.length xs + 1 == NonEmpty.length (NonEmpty.cons 0 xs)
  quickCheck $ \xs ->
    let xs' = NonEmpty.cons 0 xs -- ensure xs' has at least one element
    in NonEmpty.length xs' - 1 == S.length (NonEmpty.drop 1 xs')

  log "Test splitAt/head/last"
  quickCheck $ \idx seq ->
    let idx' :: Int
        idx' = integerBetween 0 (NonEmpty.length seq) idx

        split :: Tuple (S.Seq Number) (S.Seq Number)
        split = NonEmpty.splitAt idx' seq

    in  S.last (fst split) == NonEmpty.index (idx' - 1) seq
          && S.head (snd split) == NonEmpty.index idx' seq
          <?> ("seq: " <> show seq <> ", idx':" <> show idx')

  log "Test that adjust is safe"
  quickCheck $ \seq ->
    let f n = NonEmpty.adjust id n (seq :: NonEmpty.Seq Number)
    in f (-1) == f (NonEmpty.length seq)

  log "Test that index is safe"
  quickCheck $ \seq ->
    let f n = NonEmpty.index n (seq :: NonEmpty.Seq Number)
    in f (-1) == Nothing && f (NonEmpty.length seq) == Nothing

  log "Test inBounds"
  quickCheck $ \seq ->
    let seq' = NonEmpty.cons 0 seq
        lowerBound = 0
        upperBound = NonEmpty.length seq' - 1
    in NonEmpty.inBounds lowerBound seq' && NonEmpty.inBounds upperBound seq'
        && not (NonEmpty.inBounds (lowerBound - 1) seq')
        && not (NonEmpty.inBounds (upperBound + 1) seq')

  log "Test adjust"
  quickCheck $ \seq idx ->
    let seq' = const 0 <$> NonEmpty.cons 0 seq
        idx' = integerBetween 0 (NonEmpty.length seq') idx
        result = sum (NonEmpty.adjust (+1) idx' seq')
    in  result == 1 <?> "seq': " <> show seq' <> ", result: " <> show result

  log "Test take"
  quickCheck $ \seq n ->
    let result = S.length (NonEmpty.take n (seq :: NonEmpty.Seq Number))
    in 0 <= result && result <= abs n <?> err [ "n = " <> show n
                                              , "seq = " <> show seq
                                              , "result = " <> show result
                                              ]

  log "Test drop"
  quickCheck $ \seq n ->
    let dropped = NonEmpty.length (seq :: NonEmpty.Seq Number) - S.length (NonEmpty.drop n seq)
    in 0 <= dropped && dropped <= abs n <?> err [ "n = " <> show n
                                                , "seq = " <> show seq
                                                , "dropped = " <> show dropped
                                                ]

  log "Test filter"
  quickCheck $ \seq -> S.null (NonEmpty.filter (const false) (seq :: NonEmpty.Seq Number))
  quickCheck $ \seq -> NonEmpty.filter (const true) seq === (NonEmpty.toPlain seq :: S.Seq Number)
  quickCheck $ \seq f -> all f (NonEmpty.filter f (seq :: NonEmpty.Seq Number))

  log "Test length"
  quickCheck $ \seq -> NonEmpty.length (seq :: NonEmpty.Seq Number) >= 1 <?> show seq

  log "Test cons/uncons"
  quickCheck $ \seq x ->
    NonEmpty.uncons (NonEmpty.cons x seq) === Tuple (x :: Number) (NonEmpty.toPlain seq)
  quickCheck $ \seq x ->
    NonEmpty.unsnoc (NonEmpty.snoc seq x) === Tuple (NonEmpty.toPlain seq) (x :: Number)

  log "Test init"
  quickCheck $ \seq ->
    NonEmpty.init seq === NonEmpty.take (NonEmpty.length seq - 1) (seq :: NonEmpty.Seq Number)

  log "Test tail"
  quickCheck $ \seq ->
    NonEmpty.tail seq === NonEmpty.drop 1 (seq :: NonEmpty.Seq Number)

  log "Test head"
  quickCheck $ \seq x ->
    NonEmpty.head (NonEmpty.cons x seq) === (x :: Number)

  log "Test last"
  quickCheck $ \seq x ->
    NonEmpty.last (NonEmpty.snoc seq x) === (x :: Number)

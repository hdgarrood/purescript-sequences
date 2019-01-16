module Test.Main (main) where

import Prelude (Unit, discard)

import Effect (Effect)

import Tests.Data.Sequence          (sequenceTests)
import Tests.Data.Sequence.NonEmpty (nonEmptySequenceTests)
import Tests.Data.Sequence.Ordered  (orderedSequenceTests)

main :: Effect Unit
main = do
  sequenceTests
  nonEmptySequenceTests
  orderedSequenceTests

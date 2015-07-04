module Test.Main (main) where

import Prelude

import Tests.Data.Sequence          (sequenceTests)
import Tests.Data.Sequence.NonEmpty (nonEmptySequenceTests)
import Tests.Data.Sequence.Ordered  (orderedSequenceTests)

main = do
  sequenceTests
  nonEmptySequenceTests
  orderedSequenceTests

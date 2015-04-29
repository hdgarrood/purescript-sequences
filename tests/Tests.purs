module Tests where

import Tests.Data.Sequence (sequenceTests)
import Tests.Data.Sequence.NonEmpty (nonEmptySequenceTests)

main = do
  sequenceTests
  nonEmptySequenceTests

module Test.Plot.Label.FixLabelledPositions
  ( fixLabelledPositionsTests
  ) where

import Prelude
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Plot.Label (fixLabelledPositions)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Types (Position)

fixLabelledPositionsTests :: TestSuite
fixLabelledPositionsTests =
  suite "Plot.Label - fixLabelledPositions" do
    test "SHOULD be { x: 2, y: 2 } WHEN only labelled position is { x: 2, y: 2 }" do
      let
        position = { x: 2.0, y: 2.0 }

        labelledPosition = Tuple "test" position

        fixedLabelPosition = fixLabelledPositions alwaysOnCanvas [ labelledPosition ]

        expected = [ Tuple "test" position ]
      equal expected fixedLabelPosition
    test "SHOULD be { x: 2, y: 30 } AND { x: 2, y: 10 } WHEN labels overlap at position { x: 2, y: 30 }" do
      let
        position = { x: 2.0, y: 30.0 }

        labelledPositionA = Tuple "test" position
        labelledPositionB = Tuple "test" position

        fixedLabelPosition = fixLabelledPositions alwaysOnCanvas [ labelledPositionA, labelledPositionB ]

        expected = [ Tuple "test" position, Tuple "test" { x: 2.0, y: 10.0 } ]
      equal expected fixedLabelPosition

emptyDrawCommands :: DrawCommand Unit
emptyDrawCommands = pure unit

alwaysOnCanvas :: Position -> Boolean
alwaysOnCanvas = const false

alwaysOffCanvas :: Position -> Boolean
alwaysOffCanvas = const true

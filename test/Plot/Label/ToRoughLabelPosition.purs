module Test.Plot.Label.ToRoughLabelPosition
  ( toRoughLabelPositionTests
  ) where

import Prelude

import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Draw.Actions (drawPlotLine)
import Draw.Commands (DrawCommand)
import Plot.Label (toRoughLabelPosition)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

toRoughLabelPositionTests :: TestSuite
toRoughLabelPositionTests =
  suite "Plot.Label - toRoughLabelPosition" do
    test "SHOULD be Nothing WHEN no commands exist" do
      let
        maybePosition = toRoughLabelPosition emptyDrawCommands

        expected = Nothing
      equal expected maybePosition
    test "SHOULD be mid point of line WHEN commands have one rough line" do
      let
        lineCommand = drawPlotLine { x: 0.0, y: 0.0 } { x: 4.0, y: 4.0 } 

        maybePosition = toRoughLabelPosition $ fold [ lineCommand ]

        expected = Just { x: 2.0, y: 2.0 } 
      equal expected maybePosition

emptyDrawCommands :: DrawCommand Unit
emptyDrawCommands = fold []

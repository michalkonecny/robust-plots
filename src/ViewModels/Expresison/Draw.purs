module ViewModels.Expression.Draw where

import Prelude

import Data.Foldable (all, fold)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import IntervalArith.Misc (rationalToNumber)
import Misc.ExpectAff (ExpectAff, mapExpectAff)
import Plot.JobBatcher (hasJobs)
import Plot.Label (LabelledDrawCommand, drawRoughLabels)
import Types (Size, XYBounds, Position)
import ViewModels.Expression (ExpressionViewModel(..))
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..))
import ViewModels.Expression.Function.Draw (drawRobustOnlyFunction, drawRoughAndRobustFunction, drawRoughOnlyFunction, overwiteFunctionAccuracy, overwriteFunctionExpression)
import ViewModels.Expression.Generic (drawingStatus)

overwiteAccuracy :: Number -> AccuracyCalculator -> Int -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
overwiteAccuracy accuracyTarget toDomainAccuracy batchSegmentCount bounds (Function vm) =
  mapExpectAff Function
    $ overwiteFunctionAccuracy
        vm
        accuracyTarget
        toDomainAccuracy
        batchSegmentCount
        bounds

overwriteExpression :: Expression -> String -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
overwriteExpression expression text autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ overwriteFunctionExpression
        vm
        expression
        text
        autoRobust
        (fromPixelAccuracy size bounds)
        batchSegmentCount
        size
        bounds

drawRoughAndRobust :: Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRoughAndRobust autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ drawRoughAndRobustFunction
        (fromPixelAccuracy size bounds)
        autoRobust
        batchSegmentCount
        size
        bounds
        vm

drawRobustOnly :: Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRobustOnly batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ drawRobustOnlyFunction
        (fromPixelAccuracy size bounds)
        batchSegmentCount
        size
        bounds
        vm

drawRoughOnly :: Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRoughOnly size bounds (Function vm) =
  mapExpectAff Function
    $ drawRoughOnlyFunction
        (fromPixelAccuracy size bounds)
        size
        bounds
        vm

allRobustComplete :: Array ExpressionViewModel -> Boolean
allRobustComplete = all $ \vm -> DrawnRobust == drawingStatus vm

fromPixelAccuracy :: Size -> XYBounds -> Number -> Number
fromPixelAccuracy canvasSize bounds pixelAccuracy = pixelAccuracy * pixelToDomainRatio
  where
  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  pixelToDomainRatio = rationalToNumber $ rangeY / canvasSize.height

labelCommands :: (Position -> Boolean) -> Array ExpressionViewModel -> DrawCommand Unit
labelCommands isOffCanvas = drawRoughLabels isOffCanvas <<< map toLabelledPositions
  where
  toLabelledPositions :: ExpressionViewModel -> LabelledDrawCommand
  toLabelledPositions (Function vm) = Tuple text vm.commands.rough
    where
    { before: text } = splitAt 20 vm.name

toMaybeDrawCommand :: ExpressionViewModel -> Maybe (DrawCommand Unit)
toMaybeDrawCommand (Function plot) = case plot.expression of
  Just expression -> case plot.status of
    Off -> Nothing
    Rough -> Just plot.commands.rough
    Robust -> case plot.commands.status of
      DrawnRobust -> Just plot.commands.robust
      _ -> Just $ fold [ plot.commands.rough, plot.commands.robust ]
  Nothing -> Nothing

appendRobustDrawCommands :: DrawCommand Unit -> ExpressionViewModel -> ExpressionViewModel
appendRobustDrawCommands commands (Function vm) = Function $ vm { commands { robust = fold [ vm.commands.robust, commands ], status = status } }
  where
  status = if hasJobs vm.queue then vm.commands.status else DrawnRobust
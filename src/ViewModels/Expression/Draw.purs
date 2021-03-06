module ViewModels.Expression.Draw where

import Prelude
import Data.Foldable (all, fold)
import Data.Maybe (Maybe(..))
import Data.String (length, take)
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Misc.ExpectAff (ExpectAff, mapExpectAff)
import Plot.JobBatcher (JobQueue, hasJobs)
import Plot.Label (LabelledDrawCommand, drawRoughLabels)
import Types (Size, XYBounds, Position)
import ViewModels.Expression (ExpressionViewModel(..))
import ViewModels.Expression.Common (AccuracyCalculator, DrawingCommands, DrawingStatus(..), Status(..), fromPixelAccuracy)
import ViewModels.Expression.Function.Draw (drawRobustOnlyFunction, drawRoughAndRobustFunction, drawRoughOnlyFunction, overwriteFunctionAccuracy)
import ViewModels.Expression.Helper (drawingStatus, expressionStatus)
import ViewModels.Expression.Parametric.Draw (drawRobustOnlyParametric, drawRoughAndRobustParametric, drawRoughOnlyParametric, overwriteParametricAccuracy)

overwriteAccuracy :: Number -> AccuracyCalculator -> Int -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
overwriteAccuracy accuracyTarget toDomainAccuracy batchSegmentCount bounds (Function vm) =
  mapExpectAff Function
    $ overwriteFunctionAccuracy
        vm
        accuracyTarget
        toDomainAccuracy
        batchSegmentCount
        bounds

overwriteAccuracy accuracyTarget toDomainAccuracy batchSegmentCount bounds (Parametric vm) =
  mapExpectAff Parametric
    $ overwriteParametricAccuracy
        vm
        accuracyTarget
        toDomainAccuracy
        batchSegmentCount
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

drawRoughAndRobust autoRobust batchSegmentCount size bounds (Parametric vm) =
  mapExpectAff Parametric
    $ drawRoughAndRobustParametric
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

drawRobustOnly batchSegmentCount size bounds (Parametric vm) =
  mapExpectAff Parametric
    $ drawRobustOnlyParametric
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

drawRoughOnly size bounds (Parametric vm) =
  mapExpectAff Parametric
    $ drawRoughOnlyParametric
        (fromPixelAccuracy size bounds)
        size
        bounds
        vm

allComplete :: Array ExpressionViewModel -> Boolean
allComplete = all isRobustComplete
  where
  isRobustComplete :: ExpressionViewModel -> Boolean
  isRobustComplete vm = validRobust || validRough || validOff
    where
    dStatus = drawingStatus vm

    eStatus = expressionStatus vm

    validRobust = DrawnRobust == dStatus && Robust == eStatus

    validRough = (DrawnRough == dStatus || RobustInProgress == dStatus || DrawnRobust == dStatus) && Rough == eStatus

    validOff = Off == eStatus

labelCommands :: (Position -> Boolean) -> Array ExpressionViewModel -> DrawCommand Unit
labelCommands isOffCanvas = drawRoughLabels isOffCanvas <<< map toLabelledPositions
  where
  toLabelledPositions :: ExpressionViewModel -> LabelledDrawCommand
  toLabelledPositions (Function vm) = genericToLabelledPositions vm

  toLabelledPositions (Parametric vm) = genericToLabelledPositions vm

  genericToLabelledPositions :: forall r. { commands :: DrawingCommands, queue :: JobQueue, name :: String | r } -> LabelledDrawCommand
  genericToLabelledPositions vm = Tuple text vm.commands.rough
    where
    text = if length vm.name <= 23 then vm.name else take 20 vm.name <> "..."

toMaybeDrawCommand :: ExpressionViewModel -> Maybe (DrawCommand Unit)
toMaybeDrawCommand (Function plot) = genericToMaybeDrawCommand plot

toMaybeDrawCommand (Parametric plot) = genericToMaybeDrawCommand plot

genericToMaybeDrawCommand :: forall r a. { commands :: DrawingCommands, expression :: Maybe a, status :: Status | r } -> Maybe (DrawCommand Unit)
genericToMaybeDrawCommand plot = case plot.expression of
  Just _ -> case plot.status of
    Off -> Nothing
    Rough -> Just plot.commands.rough
    Robust -> case plot.commands.status of
      DrawnRobust -> Just plot.commands.robust
      _ -> Just $ fold [ plot.commands.rough, plot.commands.robust ]
  Nothing -> Nothing

appendRobustDrawCommands :: DrawCommand Unit -> ExpressionViewModel -> ExpressionViewModel
appendRobustDrawCommands commands (Function vm) = Function $ genericAppendRobustDrawCommands commands vm

appendRobustDrawCommands commands (Parametric vm) = Parametric $ genericAppendRobustDrawCommands commands vm

genericAppendRobustDrawCommands ::
  forall r.
  DrawCommand Unit ->
  { commands :: DrawingCommands, queue :: JobQueue, status :: Status | r } ->
  { commands :: DrawingCommands, queue :: JobQueue, status :: Status | r }
genericAppendRobustDrawCommands commands vm = vm { commands { robust = fold [ vm.commands.robust, commands ], status = status } }
  where
  status = if hasJobs vm.queue then vm.commands.status else DrawnRobust

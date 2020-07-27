module ViewModels.Expression where

import Prelude
import Control.Parallel (parSequence)
import Data.Array (cons, find, uncons)
import Data.Either (Either(..))
import Data.Foldable (all, any, fold, sum)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import IntervalArith.Misc (rationalToNumber)
import Misc.Array (alterWhere)
import Misc.ExpectAff (ExpectAff, ExpectArrayAff, MaybeExpectAff, mapExpectAff)
import Plot.JobBatcher (Job, JobResult, cancelAll, clearCancelled, countJobs, hasJobs, isCancelled, runFirst, setRunning)
import Plot.Label (LabelledDrawCommand, drawRoughLabels)
import Types (Id, Size, XYBounds, Position)
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..))
import ViewModels.Expression.Function (FunctionViewModel, newFunctionViewModel)
import ViewModels.Expression.Function.Draw (drawRobustOnlyFunction, drawRoughAndRobustFunction, drawRoughOnlyFunction, overwiteFunctionAccuracy, overwriteFunctionExpression)

data ExpressionViewModel
  = Function FunctionViewModel

expressionId :: ExpressionViewModel -> Id
expressionId (Function vm) = vm.id

drawingStatus :: ExpressionViewModel -> DrawingStatus
drawingStatus (Function vm) = vm.commands.status

expressionStatus :: ExpressionViewModel -> Status
expressionStatus (Function vm) = vm.status

expressionName :: ExpressionViewModel -> String
expressionName (Function vm) = vm.name

expressionText :: ExpressionViewModel -> String
expressionText (Function vm) = vm.expressionText

expressionAccruacy :: ExpressionViewModel -> Number
expressionAccruacy (Function vm) = vm.accuracy

overwriteStatus :: Status -> ExpressionViewModel -> ExpressionViewModel
overwriteStatus status (Function vm) = Function $ vm { status = status }

overwriteName :: String -> ExpressionViewModel -> ExpressionViewModel
overwriteName name (Function vm) = Function $ vm { name = name }

findById :: Id -> Array ExpressionViewModel -> Maybe ExpressionViewModel
findById id vms = find (\vm -> id == expressionId vm) vms

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

alterExpression :: (ExpressionViewModel -> ExpressionViewModel) -> Id -> Array ExpressionViewModel -> Array ExpressionViewModel
alterExpression alterF id = alterWhere (\e -> id == expressionId e) alterF

alterExpressionAsync :: (ExpressionViewModel -> ExpectAff ExpressionViewModel) -> Id -> Array ExpressionViewModel -> ExpectArrayAff ExpressionViewModel
alterExpressionAsync alterF id = parSequence <<< map (\e -> if id == expressionId e then alterF e else pure $ Right e)

appendRobustDrawCommands :: DrawCommand Unit -> ExpressionViewModel -> ExpressionViewModel
appendRobustDrawCommands commands (Function vm) = Function $ vm { commands { robust = fold [ vm.commands.robust, commands ], status = status } }
  where
  status = if hasJobs vm.queue then vm.commands.status else DrawnRobust

initialName :: Int -> String
initialName id = "Function " <> (show id)

newFunctionExpressionViewModel :: Id -> ExpressionViewModel
newFunctionExpressionViewModel = Function <<< newFunctionViewModel

countBatches :: Array ExpressionViewModel -> Int
countBatches = sum <<< map toBatchCount
  where
  toBatchCount :: ExpressionViewModel -> Int
  toBatchCount (Function vm) = countJobs vm.queue

queueHasJobs :: ExpressionViewModel -> Boolean
queueHasJobs (Function vm) = hasJobs vm.queue

labelCommands :: (Position -> Boolean) -> Array ExpressionViewModel -> DrawCommand Unit
labelCommands isOffCanvas = drawRoughLabels isOffCanvas <<< map toLabelledPositions
  where
  toLabelledPositions :: ExpressionViewModel -> LabelledDrawCommand
  toLabelledPositions (Function vm) = Tuple text vm.commands.rough
    where
    { before: text } = splitAt 20 vm.name

allRobustComplete :: Array ExpressionViewModel -> Boolean
allRobustComplete = all $ \vm -> DrawnRobust == drawingStatus vm

anyHasJobs :: Array ExpressionViewModel -> Boolean
anyHasJobs = any queueHasJobs

cancelAllJobs :: Array ExpressionViewModel -> Array ExpressionViewModel
cancelAllJobs = map cancel
  where
  cancel :: ExpressionViewModel -> ExpressionViewModel
  cancel (Function vm) = Function $ vm { queue = cancelAll vm.queue }

clearCancelledJobs :: Array ExpressionViewModel -> Array ExpressionViewModel
clearCancelledJobs = map clear
  where
  clear :: ExpressionViewModel -> ExpressionViewModel
  clear (Function vm) = Function $ vm { queue = clearCancelled vm.queue }

isJobCancelled :: Job -> Array ExpressionViewModel -> Boolean
isJobCancelled job = any check
  where
  check :: ExpressionViewModel -> Boolean
  check (Function vm) = isCancelled vm.queue job.id

setFirstRunningJob :: Array ExpressionViewModel -> Array ExpressionViewModel
setFirstRunningJob vms = case uncons vms of
  Nothing -> vms
  Just { head, tail } ->
    if queueHasJobs head then
      cons (go head) tail
    else
      cons head $ setFirstRunningJob tail
  where
  go :: ExpressionViewModel -> ExpressionViewModel
  go (Function vm) = Function $ vm { queue = setRunning vm.queue }

runFirstJob :: Size -> Array ExpressionViewModel -> MaybeExpectAff JobResult
runFirstJob size vms = case uncons vms of
  Nothing -> pure Nothing
  Just { head, tail } ->
    if queueHasJobs head then
      go head
    else
      runFirstJob size tail
  where
  go :: ExpressionViewModel -> MaybeExpectAff JobResult
  go (Function vm) = runFirst size vm.queue

toMaybeDrawCommand :: ExpressionViewModel -> Maybe (DrawCommand Unit)
toMaybeDrawCommand (Function plot) = case plot.expression of
  Just expression -> case plot.status of
    Off -> Nothing
    Rough -> Just plot.commands.rough
    Robust -> case plot.commands.status of
      DrawnRobust -> Just plot.commands.robust
      _ -> Just $ fold [ plot.commands.rough, plot.commands.robust ]
  Nothing -> Nothing

fromPixelAccuracy :: Size -> XYBounds -> Number -> Number
fromPixelAccuracy canvasSize bounds pixelAccuracy = pixelAccuracy * pixelToDomainRatio
  where
  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  pixelToDomainRatio = rationalToNumber $ rangeY / canvasSize.height

module ViewModels.Expression where

import Prelude
import Control.Parallel (parSequence)
import Data.Array (cons, uncons)
import Data.Either (Either(..))
import Data.Foldable (all, any, fold, sum)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Misc.Array (alterWhere)
import Misc.ExpectAff (ExpectAff, ExpectArrayAff, MaybeExpectAff, mapExpectAff)
import Plot.JobBatcher (Job, JobQueue, JobResult, cancelAll, clearCancelled, countJobs, hasJobs, initialJobQueue, isCancelled, runFirst, setRunning)
import Plot.Label (LabelledDrawCommand, drawRoughLabels)
import Types (Id, Size, XYBounds, Position)
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..))
import ViewModels.Expression.Function (FunctionViewModel, drawRobustOnlyFunction, drawRoughAndRobustFunction, drawRoughOnlyFunction, enqueueFunctionExpression, overwiteFunctionAccuracy, overwriteFunctionExpression)

data ExpressionViewModel
  = Function FunctionViewModel

enqueueExpression :: ExpressionViewModel -> Number -> Int -> XYBounds -> ExpectAff JobQueue
enqueueExpression (Function vm) accuracyTarget batchSegmentCount bounds =
  enqueueFunctionExpression
    vm
    accuracyTarget
    batchSegmentCount
    bounds

expressionId :: ExpressionViewModel -> Id
expressionId (Function vm) = vm.id

drawingStatus :: ExpressionViewModel -> DrawingStatus
drawingStatus (Function vm) = vm.commands.status

overwiteAccuracy :: ExpressionViewModel -> Number -> Int -> XYBounds -> ExpectAff ExpressionViewModel
overwiteAccuracy (Function vm) accuracyTarget batchSegmentCount bounds =
  mapExpectAff Function
    $ overwiteFunctionAccuracy
        vm
        accuracyTarget
        batchSegmentCount
        bounds

overwriteExpression :: ExpressionViewModel -> Expression -> String -> Boolean -> AccuracyCalculator -> Int -> Size -> XYBounds -> ExpectAff ExpressionViewModel
overwriteExpression (Function vm) expression text autoRobust toDomainAccuracy batchSegmentCount size bounds =
  mapExpectAff Function
    $ overwriteFunctionExpression
        vm
        expression
        text
        autoRobust
        toDomainAccuracy
        batchSegmentCount
        size
        bounds

drawRoughAndRobust :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRoughAndRobust toDomainAccuracy autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ drawRoughAndRobustFunction
        toDomainAccuracy
        autoRobust
        batchSegmentCount
        size
        bounds
        vm

drawRobustOnly :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRobustOnly toDomainAccuracy autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ drawRobustOnlyFunction
        toDomainAccuracy
        autoRobust
        batchSegmentCount
        size
        bounds
        vm

drawRoughOnly :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRoughOnly toDomainAccuracy autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ drawRoughOnlyFunction
        toDomainAccuracy
        autoRobust
        batchSegmentCount
        size
        bounds
        vm

alterExpression :: (ExpressionViewModel -> ExpressionViewModel) -> Id -> Array ExpressionViewModel -> Array ExpressionViewModel
alterExpression alterF id = alterWhere (\e -> id == expressionId e) alterF

alterExpressionAsync :: (ExpressionViewModel -> ExpectAff ExpressionViewModel) -> Id -> Array ExpressionViewModel -> ExpectArrayAff ExpressionViewModel
alterExpressionAsync alterF id = parSequence <<< map (\e -> if id == expressionId e then alterF e else pure $ Right e)

appendRobustDrawCommands :: DrawCommand Unit -> ExpressionViewModel -> ExpressionViewModel
appendRobustDrawCommands commands (Function vm) = Function $ vm { commands { robust = fold [ vm.commands.robust, commands ] } }

initialName :: Int -> String
initialName id = "Function " <> (show id)

newFunctionViewModel :: Id -> ExpressionViewModel
newFunctionViewModel id =
  Function
    $ { expressionText: ""
      , expression: Nothing
      , id
      , commands:
          { robust: pure unit
          , rough: pure unit
          , status: DrawnNone
          }
      , queue: initialJobQueue
      , status: Robust
      , name: initialName id
      , accuracy: 2.0
      }

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
runFirstJob size plots = case uncons plots of
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

module ViewModels.Expression.Function.Draw where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Misc.ExpectAff (ExpectAff, bindTo, pureRight)
import Plot.Commands (roughPlot)
import Plot.JobBatcher (JobQueue, addPlot, cancelAll)
import Plot.PlotController (computePlotAsync)
import Types (Size, XYBounds)
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..))
import ViewModels.Expression.Function (FunctionViewModel, initialName)

enqueueFunctionExpression :: FunctionViewModel -> Number -> Int -> XYBounds -> ExpectAff JobQueue
enqueueFunctionExpression vm accuracyTarget batchSegmentCount bounds = case vm.expression of
  Nothing -> pure $ Right $ cancelAll vm.queue
  Just expression -> addPlot accuracyTarget batchSegmentCount (cancelAll vm.queue) bounds expression vm.expressionText vm.id

withExpression :: FunctionViewModel -> (Expression -> ExpectAff FunctionViewModel) -> ExpectAff FunctionViewModel
withExpression vm op = case vm.expression of
  Nothing -> pure $ Right vm
  Just expression -> op expression

overwiteFunctionAccuracy :: FunctionViewModel -> Number -> AccuracyCalculator -> Int -> XYBounds -> ExpectAff FunctionViewModel
overwiteFunctionAccuracy vm accuracyTarget toDomainAccuracy batchSegmentCount bounds = bindTo addRobustToQueue (pureRight <<< overwiteQueue)
  where
  addRobustToQueue =
    if startRobust then
      enqueueFunctionExpression vm (toDomainAccuracy accuracyTarget) batchSegmentCount bounds
    else
      pure $ Right $ cancelAll vm.queue

  startRobust = vm.status == Robust && (vm.commands.status == DrawnRobust || vm.commands.status == RobustInProgress)

  status = if startRobust && isJust vm.expression then RobustInProgress else DrawnRough

  overwiteQueue :: JobQueue -> FunctionViewModel
  overwiteQueue queue =
    vm
      { commands
        { robust = pure unit
        , status = status
        }
      , queue = queue
      , accuracy = accuracyTarget
      }

overwriteFunctionExpression :: FunctionViewModel -> Expression -> String -> Boolean -> AccuracyCalculator -> Int -> Size -> XYBounds -> ExpectAff FunctionViewModel
overwriteFunctionExpression vm expression text autoRobust toDomainAccuracy batchSegmentCount size bounds =
  bindTo computeRoughCommands
    $ \newRoughCommands ->
        bindTo addRobustToQueue
          (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
  where
  computeRoughCommands = computePlotAsync size $ roughPlot bounds expression text

  clearedQueue = cancelAll vm.queue

  addRobustToQueue =
    if autoRobust then
      addPlot (toDomainAccuracy vm.accuracy) batchSegmentCount clearedQueue bounds expression text vm.id
    else
      pure $ Right clearedQueue

  overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> FunctionViewModel
  overwriteCommandsAndQueue newRoughCommands queue =
    vm
      { expressionText = text
      , expression = Just expression
      , commands
        { rough = newRoughCommands
        , robust = pure unit
        , status = status
        }
      , queue = queue
      , name = name
      }
    where
    status = if autoRobust then RobustInProgress else DrawnRough

    name = if vm.name == initialName vm.id || vm.name == vm.expressionText then text else vm.name

drawRoughAndRobustFunction :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> FunctionViewModel -> ExpectAff FunctionViewModel
drawRoughAndRobustFunction toDomainAccuracy autoRobust batchSegmentCount size bounds vm = withExpression vm go
  where
  go :: Expression -> ExpectAff FunctionViewModel
  go expression =
    bindTo computeRoughCommands
      $ \newRoughCommands ->
          bindTo addRobustToQueue
            (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
    where
    computeRoughCommands = computePlotAsync size $ roughPlot bounds expression vm.expressionText

    clearedQueue = cancelAll vm.queue

    addRobustToQueue =
      if autoRobust && vm.status == Robust then
        addPlot (toDomainAccuracy vm.accuracy) batchSegmentCount clearedQueue bounds expression vm.expressionText vm.id
      else
        pure $ Right $ clearedQueue

    overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> FunctionViewModel
    overwriteCommandsAndQueue drawCommands queue =
      vm
        { queue = queue
        , commands
          { rough = drawCommands
          , robust = pure unit
          , status = status
          }
        }
      where
      status =
        if autoRobust && vm.status == Robust then
          RobustInProgress
        else
          DrawnRough

drawRobustOnlyFunction :: AccuracyCalculator -> Int -> Size -> XYBounds -> FunctionViewModel -> ExpectAff FunctionViewModel
drawRobustOnlyFunction toDomainAccuracy batchSegmentCount size bounds vm
  | vm.commands.status == DrawnRobust || vm.status /= Robust = pure $ Right vm
  | otherwise = withExpression vm go
    where
    go :: Expression -> ExpectAff FunctionViewModel
    go expression =
      bindTo computeRoughCommands
        $ \newRoughCommands ->
            bindTo addRobustToQueue
              (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
      where
      addRobustToQueue = addPlot (toDomainAccuracy vm.accuracy) batchSegmentCount (cancelAll vm.queue) bounds expression vm.expressionText vm.id

      computeRoughCommands = computePlotAsync size $ roughPlot bounds expression vm.expressionText

      overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> FunctionViewModel
      overwriteCommandsAndQueue drawCommands queue =
        vm
          { queue = queue
          , commands
            { rough = drawCommands
            , robust = pure unit
            , status = RobustInProgress
            }
          }

drawRoughOnlyFunction :: AccuracyCalculator -> Size -> XYBounds -> FunctionViewModel -> ExpectAff FunctionViewModel
drawRoughOnlyFunction toDomainAccuracy size bounds vm = withExpression vm go
  where
  go :: Expression -> ExpectAff FunctionViewModel
  go expression = bindTo computeRoughCommands (pureRight <<< overwriteCommands)
    where
    computeRoughCommands = computePlotAsync size $ roughPlot bounds expression vm.expressionText

    overwriteCommands :: DrawCommand Unit -> FunctionViewModel
    overwriteCommands drawCommands =
      vm
        { queue = cancelAll vm.queue
        , commands
          { rough = drawCommands
          , robust = pure unit
          , status = DrawnRough
          }
        }

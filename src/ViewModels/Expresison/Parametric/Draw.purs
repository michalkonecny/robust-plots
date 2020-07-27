module ViewModels.Expression.Parametric.Draw where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Misc.ExpectAff (ExpectAff, bindTo, pureRight)
import Plot.Commands (roughParametricPlot)
import Plot.JobBatcher (JobQueue, cancelAll)
import Plot.PlotController (computePlotAsync)
import Types (Size, XYBounds)
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..))
import ViewModels.Expression.Parametric (ParametricViewModel)

enqueueFunctionExpression :: ParametricViewModel -> Number -> Int -> XYBounds -> ExpectAff JobQueue
enqueueFunctionExpression vm accuracyTarget batchSegmentCount bounds = case vm.expression of
  Nothing -> pure $ Right $ cancelAll vm.queue
  Just expression -> pure $ Right $ cancelAll vm.queue -- TODO: Add parametric robust to Queue

withExpression :: ParametricViewModel -> ({ x :: Expression, y :: Expression } -> ExpectAff ParametricViewModel) -> ExpectAff ParametricViewModel
withExpression vm op = case vm.expression of
  Nothing -> pure $ Right vm
  Just expression -> op expression

overwiteFunctionAccuracy :: ParametricViewModel -> Number -> AccuracyCalculator -> Int -> XYBounds -> ExpectAff ParametricViewModel
overwiteFunctionAccuracy vm accuracyTarget toDomainAccuracy batchSegmentCount bounds = bindTo addRobustToQueue (pureRight <<< overwiteQueue)
  where
  addRobustToQueue =
    if startRobust then
      enqueueFunctionExpression vm (toDomainAccuracy accuracyTarget) batchSegmentCount bounds
    else
      pure $ Right $ cancelAll vm.queue

  startRobust = vm.status == Robust && (vm.commands.status == DrawnRobust || vm.commands.status == RobustInProgress)

  status = if startRobust && isJust vm.expression then RobustInProgress else DrawnRough

  overwiteQueue :: JobQueue -> ParametricViewModel
  overwiteQueue queue =
    vm
      { commands
        { robust = pure unit
        , status = status
        }
      , queue = queue
      , accuracy = accuracyTarget
      }

overwriteFunctionExpression :: ParametricViewModel -> Expression -> Expression -> String -> String -> Boolean -> AccuracyCalculator -> Int -> Size -> XYBounds -> ExpectAff ParametricViewModel
overwriteFunctionExpression vm xExpression yExpression xText yText autoRobust toDomainAccuracy batchSegmentCount size bounds =
  bindTo computeRoughCommands
    $ \newRoughCommands ->
        bindTo addRobustToQueue
          (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
  where
  computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain xExpression yExpression

  clearedQueue = cancelAll vm.queue

  addRobustToQueue =
    if autoRobust then
      pure $ Right clearedQueue -- TODO: Add parametric robust to Queue
    else
      pure $ Right clearedQueue

  overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> ParametricViewModel
  overwriteCommandsAndQueue newRoughCommands queue =
    vm
      { xExpressionText = xText
      , yExpressionText = yText
      , expression = Just { x: xExpression, y: yExpression }
      , commands
        { rough = newRoughCommands
        , robust = pure unit
        , status = status
        }
      , queue = queue
      }
    where
    status = if autoRobust then RobustInProgress else DrawnRough

drawRoughAndRobustFunction :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> ParametricViewModel -> ExpectAff ParametricViewModel
drawRoughAndRobustFunction toDomainAccuracy autoRobust batchSegmentCount size bounds vm = withExpression vm go
  where
  go :: { x :: Expression, y :: Expression } -> ExpectAff ParametricViewModel
  go { x, y } =
    bindTo computeRoughCommands
      $ \newRoughCommands ->
          bindTo addRobustToQueue
            (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
    where
    computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain x y

    clearedQueue = cancelAll vm.queue

    addRobustToQueue =
      if autoRobust && vm.status == Robust then
        pure $ Right clearedQueue -- TODO: Add parametric robust to Queue
      else
        pure $ Right clearedQueue

    overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> ParametricViewModel
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

drawRobustOnlyFunction :: AccuracyCalculator -> Int -> Size -> XYBounds -> ParametricViewModel -> ExpectAff ParametricViewModel
drawRobustOnlyFunction toDomainAccuracy batchSegmentCount size bounds vm
  | vm.commands.status == DrawnRobust || vm.status /= Robust = pure $ Right vm
  | otherwise = withExpression vm go
    where
    go :: { x :: Expression, y :: Expression } -> ExpectAff ParametricViewModel
    go { x, y } =
      bindTo computeRoughCommands
        $ \newRoughCommands ->
            bindTo addRobustToQueue
              (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
      where
      addRobustToQueue = pure $ Right (cancelAll vm.queue) -- TODO: Add parametric robust to Queue

      computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain x y

      overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> ParametricViewModel
      overwriteCommandsAndQueue drawCommands queue =
        vm
          { queue = queue
          , commands
            { rough = drawCommands
            , robust = pure unit
            , status = RobustInProgress
            }
          }

drawRoughOnlyFunction :: AccuracyCalculator -> Size -> XYBounds -> ParametricViewModel -> ExpectAff ParametricViewModel
drawRoughOnlyFunction toDomainAccuracy size bounds vm = withExpression vm go
  where
  go :: { x :: Expression, y :: Expression } -> ExpectAff ParametricViewModel
  go { x, y } = bindTo computeRoughCommands (pureRight <<< overwriteCommands)
    where
    computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain x y

    overwriteCommands :: DrawCommand Unit -> ParametricViewModel
    overwriteCommands drawCommands =
      vm
        { queue = cancelAll vm.queue
        , commands
          { rough = drawCommands
          , robust = pure unit
          , status = DrawnRough
          }
        }

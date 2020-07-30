module ViewModels.Expression.Parametric.Draw where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Draw.Commands (DrawCommand)
import Misc.ExpectAff (ExpectAff, bindTo, pureRight)
import Plot.Commands (roughParametricPlot)
import Plot.JobBatcher (JobQueue, addParametricPlot, cancelAll)
import Plot.PlotController (computePlotAsync)
import Types (Size, XYBounds, Bounds)
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..))
import ViewModels.Expression.Parametric (ParametricExpression, ParametricViewModel, ParametricExpressionText)

enqueueParametricExpression :: ParametricViewModel -> Number -> Int -> XYBounds -> ExpectAff JobQueue
enqueueParametricExpression vm accuracyTarget batchSegmentCount bounds = case vm.expression of
  Nothing -> pure $ Right $ cancelAll vm.queue
  Just { xExpression, yExpression } -> addParametricPlot accuracyTarget batchSegmentCount (cancelAll vm.queue) bounds (vm.domain) xExpression yExpression vm.id

withExpression :: ParametricViewModel -> (ParametricExpression -> ExpectAff ParametricViewModel) -> ExpectAff ParametricViewModel
withExpression vm op = case vm.expression of
  Nothing -> pure $ Right vm
  Just expression -> op expression

overwriteParametricAccuracy :: ParametricViewModel -> Number -> AccuracyCalculator -> Int -> XYBounds -> ExpectAff ParametricViewModel
overwriteParametricAccuracy vm accuracyTarget toDomainAccuracy batchSegmentCount bounds = bindTo addRobustToQueue (pureRight <<< overwriteQueue)
  where
  addRobustToQueue =
    if startRobust then
      enqueueParametricExpression vm (toDomainAccuracy accuracyTarget) batchSegmentCount bounds
    else
      pure $ Right $ cancelAll vm.queue

  startRobust = vm.status == Robust && (vm.commands.status == DrawnRobust || vm.commands.status == RobustInProgress)

  status = if startRobust && isJust vm.expression then RobustInProgress else DrawnRough

  overwriteQueue :: JobQueue -> ParametricViewModel
  overwriteQueue queue =
    vm
      { commands
        { robust = pure unit
        , status = status
        }
      , queue = queue
      , accuracy = accuracyTarget
      }

overwriteParametricExpression :: ParametricViewModel -> ParametricExpression -> ParametricExpressionText -> Boolean -> AccuracyCalculator -> Int -> Size -> XYBounds -> ExpectAff ParametricViewModel
overwriteParametricExpression vm { xExpression, yExpression } { xText, yText } autoRobust toDomainAccuracy batchSegmentCount size bounds =
  bindTo computeRoughCommands
    $ \newRoughCommands ->
        bindTo addRobustToQueue
          (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
  where
  computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain xExpression yExpression

  clearedQueue = cancelAll vm.queue

  addRobustToQueue =
    if autoRobust then
      addParametricPlot (toDomainAccuracy vm.accuracy) batchSegmentCount clearedQueue bounds (vm.domain) xExpression yExpression vm.id
    else
      pure $ Right clearedQueue

  overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> ParametricViewModel
  overwriteCommandsAndQueue newRoughCommands queue =
    vm
      { text = { xText, yText }
      , expression = Just { xExpression, yExpression }
      , commands
        { rough = newRoughCommands
        , robust = pure unit
        , status = status
        }
      , queue = queue
      }
    where
    status = if autoRobust then RobustInProgress else DrawnRough

overwriteParametricDomain :: ParametricViewModel -> Bounds -> Boolean -> AccuracyCalculator -> Int -> Size -> XYBounds -> ExpectAff ParametricViewModel
overwriteParametricDomain vm newDomain autoRobust toDomainAccuracy batchSegmentCount size bounds = withExpression vm go
  where
  go :: ParametricExpression -> ExpectAff ParametricViewModel
  go { xExpression, yExpression } =
    bindTo computeRoughCommands
      $ \newRoughCommands ->
          bindTo addRobustToQueue
            (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
    where
    computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds newDomain xExpression yExpression

    clearedQueue = cancelAll vm.queue

    addRobustToQueue =
      if autoRobust then
        addParametricPlot (toDomainAccuracy vm.accuracy) batchSegmentCount clearedQueue bounds newDomain xExpression yExpression vm.id
      else
        pure $ Right clearedQueue

    overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> ParametricViewModel
    overwriteCommandsAndQueue newRoughCommands queue =
      vm
        { commands
          { rough = newRoughCommands
          , robust = pure unit
          , status = status
          }
        , queue = queue
        , domain = newDomain
        }
      where
      status = if autoRobust then RobustInProgress else DrawnRough

drawRoughAndRobustParametric :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> ParametricViewModel -> ExpectAff ParametricViewModel
drawRoughAndRobustParametric toDomainAccuracy autoRobust batchSegmentCount size bounds vm = withExpression vm go
  where
  go :: ParametricExpression -> ExpectAff ParametricViewModel
  go { xExpression, yExpression } =
    bindTo computeRoughCommands
      $ \newRoughCommands ->
          bindTo addRobustToQueue
            (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
    where
    computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain xExpression yExpression

    clearedQueue = cancelAll vm.queue

    addRobustToQueue =
      if autoRobust && vm.status == Robust then
        addParametricPlot (toDomainAccuracy vm.accuracy) batchSegmentCount clearedQueue bounds (vm.domain) xExpression yExpression vm.id
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

drawRobustOnlyParametric :: AccuracyCalculator -> Int -> Size -> XYBounds -> ParametricViewModel -> ExpectAff ParametricViewModel
drawRobustOnlyParametric toDomainAccuracy batchSegmentCount size bounds vm
  | vm.commands.status == DrawnRobust || vm.status /= Robust = pure $ Right vm
  | otherwise = withExpression vm go
    where
    go :: ParametricExpression -> ExpectAff ParametricViewModel
    go { xExpression, yExpression } =
      bindTo computeRoughCommands
        $ \newRoughCommands ->
            bindTo addRobustToQueue
              (pureRight <<< overwriteCommandsAndQueue newRoughCommands)
      where
      addRobustToQueue = addParametricPlot (toDomainAccuracy vm.accuracy) batchSegmentCount (cancelAll vm.queue) bounds (vm.domain) xExpression yExpression vm.id

      computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain xExpression yExpression

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

drawRoughOnlyParametric :: AccuracyCalculator -> Size -> XYBounds -> ParametricViewModel -> ExpectAff ParametricViewModel
drawRoughOnlyParametric toDomainAccuracy size bounds vm = withExpression vm go
  where
  go :: ParametricExpression -> ExpectAff ParametricViewModel
  go { xExpression, yExpression } = bindTo computeRoughCommands (pureRight <<< overwriteCommands)
    where
    computeRoughCommands = computePlotAsync size $ roughParametricPlot bounds vm.domain xExpression yExpression

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

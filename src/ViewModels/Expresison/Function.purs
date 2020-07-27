module ViewModels.Expression.Function where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Misc.ExpectAff (ExpectAff)
import Plot.Commands (roughPlot)
import Plot.JobBatcher (JobQueue, addPlot, cancelAll)
import Plot.PlotController (computePlotAsync)
import Types (XYBounds, Size)
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..), defaultPlotName, handleError)

type FunctionViewModel
  = { expression :: Maybe Expression
    , expressionText :: String
    , commands ::
        { robust :: DrawCommand Unit
        , rough :: DrawCommand Unit
        , status :: DrawingStatus
        }
    , id :: Int
    , queue :: JobQueue
    , status :: Status
    , name :: String
    , accuracy :: Number
    }

enqueueFunctionExpression :: FunctionViewModel -> Number -> Int -> XYBounds -> ExpectAff JobQueue
enqueueFunctionExpression vm accuracyTarget batchSegmentCount bounds = case vm.expression of
  Nothing -> pure $ Right $ cancelAll vm.queue
  Just expression -> addPlot accuracyTarget batchSegmentCount (cancelAll vm.queue) bounds expression vm.expressionText vm.id

overwiteFunctionAccuracy :: FunctionViewModel -> Number -> Int -> XYBounds -> ExpectAff FunctionViewModel
overwiteFunctionAccuracy vm accuracyTarget batchSegmentCount bounds = handleError addRobustToQueue (pure <<< Right <<< overwiteQueue)
  where
  addRobustToQueue =
    if startRobust then
      enqueueFunctionExpression vm accuracyTarget batchSegmentCount bounds
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
  handleError computeRoughCommands
    $ \newRoughCommands ->
        handleError addRobustToQueue
          (pure <<< Right <<< overwriteCommandsAndQueue newRoughCommands)
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

    name = if vm.name == defaultPlotName vm.id || vm.name == vm.expressionText then text else vm.name

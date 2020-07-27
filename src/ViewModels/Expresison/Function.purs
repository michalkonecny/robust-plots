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
overwiteFunctionAccuracy plot accuracyTarget batchSegmentCount bounds = handleError addRobustToQueue (pure <<< Right <<< overwiteQueue)
  where
  addRobustToQueue =
    if startRobust then
      enqueueFunctionExpression plot accuracyTarget batchSegmentCount bounds
    else
      pure $ Right $ cancelAll plot.queue

  startRobust = plot.status == Robust && (plot.commands.status == DrawnRobust || plot.commands.status == RobustInProgress)

  status = if startRobust && isJust plot.expression then RobustInProgress else DrawnRough

  overwiteQueue :: JobQueue -> FunctionViewModel
  overwiteQueue queue =
    plot
      { commands
        { robust = pure unit
        , status = status
        }
      , queue = queue
      , accuracy = accuracyTarget
      }

overwriteFunctionExpression :: FunctionViewModel -> Expression -> String -> Boolean -> AccuracyCalculator -> Int -> Size -> XYBounds -> ExpectAff FunctionViewModel
overwriteFunctionExpression plot expression text autoRobust toDomainAccuracy batchSegmentCount size bounds =
  handleError roughCommands
    $ \newRoughCommands ->
        handleError addRobustToQueue
          $ (pure <<< Right <<< overwriteCommandsAndQueue newRoughCommands)
  where
  roughCommands = computePlotAsync size (roughPlot bounds expression text)

  addRobustToQueue =
    if autoRobust then
      addPlot (toDomainAccuracy plot.accuracy) batchSegmentCount (cancelAll plot.queue) bounds expression text plot.id
    else
      pure $ Right $ cancelAll plot.queue

  status = if autoRobust then RobustInProgress else DrawnRough

  name = if plot.name == defaultPlotName plot.id || plot.name == plot.expressionText then text else plot.name

  overwriteCommandsAndQueue :: DrawCommand Unit -> JobQueue -> FunctionViewModel
  overwriteCommandsAndQueue newRoughCommands queue =
    plot
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

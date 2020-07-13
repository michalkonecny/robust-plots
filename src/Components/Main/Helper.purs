module Components.Main.Helper where

import Prelude

import Components.ExpressionInput (Status(..))
import Components.ExpressionManager.Types (DrawingStatus(..), ExpressionPlot)
import Components.Main.Types (State)
import Control.Parallel (parSequence)
import Data.Array (cons, fold, foldl, mapMaybe, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff)
import IntervalArith.Misc (rationalToNumber)
import Misc.Array (alterWhere)
import Plot.Commands (roughPlot)
import Plot.JobBatcher (Job, JobResult, addPlot, cancelAll, clearCancelled, hasJobs, initialJobQueue, isCancelled, runFirst, setRunning)
import Plot.Label (LabelledDrawCommand, drawRoughLabels, textHeight)
import Plot.PlotController (computePlotAsync)
import Types (Bounds, Id, Size, XYBounds, Position)

newPlot :: Int -> ExpressionPlot
newPlot id =
  { expressionText: ""
  , expression: Nothing
  , id
  , commands:
      { robust: pure unit
      , rough: pure unit
      , status: DrawnNone
      }
  , queue: initialJobQueue
  , status: Robust
  , name: "Plot " <> (show id)
  , accuracy: 0.1
  }

updateExpressionPlotCommands :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotCommands commands plot = plot { commands { robust = fold [ plot.commands.robust, commands ] } }

alterPlot :: (ExpressionPlot -> ExpressionPlot) -> Id -> Array ExpressionPlot -> Array ExpressionPlot
alterPlot alterF id = alterWhere (\p -> p.id == id) alterF

queueHasJobs :: ExpressionPlot -> Boolean
queueHasJobs plot = hasJobs plot.queue

toLabelledPositions :: Array ExpressionPlot -> Array LabelledDrawCommand
toLabelledPositions = map (\p -> Tuple p.expressionText p.commands.rough)

labelCommands :: (Position -> Boolean) -> Array ExpressionPlot -> DrawCommand Unit
labelCommands isOffCanvas = drawRoughLabels isOffCanvas <<< toLabelledPositions

anyPlotHasJobs :: Array ExpressionPlot -> Boolean
anyPlotHasJobs = anyPlotExpression queueHasJobs

isAllRobustPlotsComplete :: Array ExpressionPlot -> Boolean
isAllRobustPlotsComplete = allPlotExpression $ \plot -> plot.commands.status == DrawnRobust

allPlotExpression :: (ExpressionPlot -> Boolean) -> Array ExpressionPlot -> Boolean
allPlotExpression f = (foldl (&&) true) <<< (map f)

anyPlotExpression :: (ExpressionPlot -> Boolean) -> Array ExpressionPlot -> Boolean
anyPlotExpression f = (foldl (||) false) <<< (map f)

cancelAllPlotJobs :: Array ExpressionPlot -> Array ExpressionPlot
cancelAllPlotJobs = map (\plot -> plot { queue = cancelAll plot.queue })

clearAllCancelled :: Array ExpressionPlot -> Array ExpressionPlot
clearAllCancelled = map (\plot -> plot { queue = clearCancelled plot.queue })

isCancelledInAnyPlot :: Job -> Array ExpressionPlot -> Boolean
isCancelledInAnyPlot job = anyPlotExpression (\plot -> isCancelled plot.queue job.id)

setFirstRunningJob :: Array ExpressionPlot -> Array ExpressionPlot
setFirstRunningJob plots = case uncons plots of
  Nothing -> plots
  Just { head, tail } ->
    if queueHasJobs head then
      cons (head { queue = setRunning head.queue }) tail
    else
      cons head $ setFirstRunningJob tail

runFirstJob :: Size -> Bounds -> Array ExpressionPlot -> Aff (Maybe JobResult)
runFirstJob size xBounds plots = case uncons plots of
  Nothing -> pure Nothing
  Just { head, tail } ->
    if queueHasJobs head then
      runFirst size xBounds head.queue
    else
      runFirstJob size xBounds tail

toMaybeDrawCommand :: ExpressionPlot -> Maybe (DrawCommand Unit)
toMaybeDrawCommand plot = case plot.expression of
  Just expression -> case plot.status of
    Off -> Nothing
    Rough -> Just plot.commands.rough
    Robust -> Just $ fold [ plot.commands.rough, plot.commands.robust ]
  Nothing -> Nothing

foldDrawCommands :: State -> DrawCommand Unit
foldDrawCommands state = fold ([ state.clearPlot ] <> (mapMaybe toMaybeDrawCommand state.plots) <> [ labelCommands (isOffCanvasCheck state.input.size) state.plots ])

isOffCanvasCheck :: Size -> Position -> Boolean
isOffCanvasCheck canvasSize position = position.x < textHeight || position.x > width || position.y < textHeight || position.y > height
  where
    width = rationalToNumber canvasSize.width
    height = (rationalToNumber canvasSize.height) - 5.0

clearAddPlotCommands :: Boolean -> Int -> Size -> XYBounds -> Array ExpressionPlot -> Aff (Array ExpressionPlot)
clearAddPlotCommands autoRobust batchCount size newBounds = parSequence <<< (map clearAddPlot)
  where
  toDomainAccuracy :: Number -> Number
  toDomainAccuracy = fromPixelAccuracy size newBounds

  clearAddPlot :: ExpressionPlot -> Aff ExpressionPlot
  clearAddPlot plot = case plot.expression of
    Nothing -> pure plot
    Just expression -> do
      drawCommands <- computePlotAsync size $ roughPlot newBounds expression plot.expressionText
      pure $ plot { queue = queue, commands { rough = drawCommands, robust = pure unit, status = status } }
      where
      queue =
        if autoRobust && plot.status == Robust then
          addPlot (toDomainAccuracy plot.accuracy) batchCount (cancelAll plot.queue) newBounds expression plot.expressionText plot.id
        else
          cancelAll plot.queue

      status =
        if autoRobust && plot.status == Robust then
          RobustInProgress
        else
          DrawnRough

fromPixelAccuracy :: Size -> XYBounds -> Number -> Number
fromPixelAccuracy canvasSize bounds pixelAccuracy = pixelAccuracy * pixelToDomainRatio
  where
  rangeY = bounds.yBounds.upper - bounds.yBounds.lower

  pixelToDomainRatio = rationalToNumber $ rangeY / canvasSize.height

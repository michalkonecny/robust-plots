module Components.Main.Helper where

import Prelude
import Components.ExpressionManager.Types (ExpressionPlot)
import Components.Main.Types (State)
import Control.Parallel (parSequence)
import Data.Array (cons, fold, foldl, uncons)
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff)
import Misc.Array (alterWhere)
import Plot.Commands (roughPlot)
import Plot.JobBatcher (Job, JobResult, addPlot, cancelAll, clearCancelled, hasJobs, initialJobQueue, isCancelled, runFirst, setRunning)
import Plot.PlotController (computePlotAsync)
import Types (Id, Size, XYBounds, Bounds)

newPlot :: Int -> ExpressionPlot
newPlot id =
  { expressionText: ""
  , expression: Nothing
  , id
  , robustDrawCommands: pure unit
  , roughDrawCommands: pure unit
  , queue: initialJobQueue
  , name: "Plot " <> (show id)
  , accuracy: 0.1
  , showRough: true
  , showRobust: true
  }

updateExpressionPlotCommands :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotCommands commands plot = plot { robustDrawCommands = fold [ plot.robustDrawCommands, commands ] }

alterPlot :: (ExpressionPlot -> ExpressionPlot) -> Id -> Array ExpressionPlot -> Array ExpressionPlot
alterPlot alterF id = alterWhere (\p -> p.id == id) alterF

queueHasJobs :: ExpressionPlot -> Boolean
queueHasJobs plot = hasJobs plot.queue

anyPlotHasJobs :: Array ExpressionPlot -> Boolean
anyPlotHasJobs = anyPlotExpression queueHasJobs

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

toDrawCommand :: ExpressionPlot -> DrawCommand Unit
toDrawCommand plot = case plot.expression of
  Just expression -> case plot.showRough, plot.showRobust of
    false, false -> pure unit
    true, false -> plot.roughDrawCommands
    false, true -> plot.robustDrawCommands
    true, true ->  fold [ plot.roughDrawCommands, plot.robustDrawCommands ]
  Nothing -> pure unit

foldDrawCommands :: State -> DrawCommand Unit
foldDrawCommands state = fold $ [ state.clearPlot ] <> map toDrawCommand state.plots

clearAddPlotCommands :: Int -> Size -> XYBounds -> Array ExpressionPlot -> Aff (Array ExpressionPlot)
clearAddPlotCommands batchCount size newBounds = parSequence <<< (map clearAddPlot)
  where
  clearAddPlot :: ExpressionPlot -> Aff ExpressionPlot
  clearAddPlot plot = case plot.expression of
    Nothing -> pure plot
    Just expression -> do
      let
        cancelledQueue = cancelAll plot.queue

        queueWithPlot = addPlot plot.accuracy batchCount cancelledQueue newBounds expression plot.expressionText plot.id
      drawCommands <- computePlotAsync size $ roughPlot newBounds expression plot.expressionText
      pure $ plot { queue = queueWithPlot, roughDrawCommands = drawCommands, robustDrawCommands = pure unit }

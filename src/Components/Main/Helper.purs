module Components.Main.Helper where

import Prelude
import Components.ExpressionInput (Status(..))
import Components.Main.Types (ExpressionPlot, State)
import Control.Parallel (parSequence)
import Data.Array (cons, fold, foldl, mapMaybe, uncons)
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff)
import IntervalArith.Misc (Rational)
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
  , status: Robust
  }

updateExpressionPlotCommands :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotCommands commands plot = plot { robustDrawCommands = fold [ plot.robustDrawCommands, commands ] }

alterPlot :: (ExpressionPlot -> ExpressionPlot) -> Id -> Array ExpressionPlot -> Array ExpressionPlot
alterPlot alterF id = map mapper
  where
  mapper :: ExpressionPlot -> ExpressionPlot
  mapper plot = if plot.id == id then alterF plot else plot

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

toMaybeDrawCommand :: ExpressionPlot -> Maybe (DrawCommand Unit)
toMaybeDrawCommand plot = case plot.expression of
  Just expression -> case plot.status of
    Off -> Nothing
    Rough -> Just plot.roughDrawCommands
    Robust -> Just $ fold [ plot.roughDrawCommands, plot.robustDrawCommands ]
  Nothing -> Nothing

foldDrawCommands :: State -> DrawCommand Unit
foldDrawCommands state = fold $ [ state.clearPlot ] <> mapMaybe toMaybeDrawCommand state.plots

clearAddPlotCommands :: Number -> Int -> Size -> XYBounds -> Array ExpressionPlot -> Aff (Array ExpressionPlot)
clearAddPlotCommands accuracy batchCount size newBounds = parSequence <<< (map clearAddPlot)
  where
  clearAddPlot :: ExpressionPlot -> Aff ExpressionPlot
  clearAddPlot plot = case plot.expression of
    Nothing -> pure plot
    Just expression -> do
      let
        cancelledQueue = cancelAll plot.queue

        queueWithPlot = addPlot accuracy batchCount cancelledQueue newBounds expression plot.expressionText plot.id
      drawCommands <- computePlotAsync size $ roughPlot newBounds expression plot.expressionText
      pure $ plot { queue = queueWithPlot, roughDrawCommands = drawCommands, robustDrawCommands = pure unit }

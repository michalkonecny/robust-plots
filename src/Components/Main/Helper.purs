module Components.Main.Helper where

import Prelude

import Components.ExpressionInput (Status(..))
import Components.Main.Types (ExpressionPlot, State)
import Data.Array (cons, fold, foldl, mapMaybe, uncons)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Effect.Aff (Aff)
import IntervalArith.Misc (Rational)
import Plot.Commands (PlotCommand, robustPlot)
import Plot.JobBatcher (Job, JobResult, cancelAll, clearCancelled, hasJobs, initialJobQueue, isCancelled, runFirst, setRunning)
import Types (Id, Size, XYBounds, Bounds)

newPlot :: Int -> ExpressionPlot
newPlot id = { expressionText: "", expression: Nothing, id, drawCommands: pure unit, queue: initialJobQueue, status: Robust }

updateExpressionPlotCommands :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotCommands commands plot = plot { drawCommands = fold [ plot.drawCommands, commands ] }

alterPlot :: (ExpressionPlot -> ExpressionPlot) -> Id -> Array ExpressionPlot -> Array ExpressionPlot
alterPlot alterF id = map mapper
  where
  mapper :: ExpressionPlot -> ExpressionPlot
  mapper plot = if plot.id == id then alterF plot else plot

initialBounds :: XYBounds
initialBounds = xyBounds (-one) one (-one) one

toMaybePlotCommandWithId :: Int -> XYBounds -> ExpressionPlot -> Maybe (Tuple PlotCommand Id)
toMaybePlotCommandWithId segmentCount newBounds plot = case plot.expression of
  Just expression -> Just $ Tuple (robustPlot segmentCount newBounds expression plot.expressionText) plot.id
  Nothing -> Nothing

queueHasJobs :: ExpressionPlot -> Boolean
queueHasJobs plot = hasJobs plot.queue

anyPlotHasJobs :: Array ExpressionPlot -> Boolean
anyPlotHasJobs = trueInAnyPlotExpression queueHasJobs

trueInAnyPlotExpression :: (ExpressionPlot -> Boolean) -> Array ExpressionPlot -> Boolean
trueInAnyPlotExpression f = (foldl (||) false) <<< (map f)

cancelAllPlotJobs :: Array ExpressionPlot -> Array ExpressionPlot
cancelAllPlotJobs = map (\plot -> plot { queue = cancelAll plot.queue })

clearAllCancelled :: Array ExpressionPlot -> Array ExpressionPlot
clearAllCancelled = map (\plot -> plot { queue = clearCancelled plot.queue })

isCancelledInAnyPlot :: Job -> Array ExpressionPlot -> Boolean
isCancelledInAnyPlot job = trueInAnyPlotExpression (\plot -> isCancelled plot.queue job.id)

setFirstRunningJob :: Array ExpressionPlot -> Array ExpressionPlot
setFirstRunningJob plots = case uncons plots of
  Nothing -> plots
  Just { head, tail } ->
    if queueHasJobs head then
      cons (head { queue = setRunning head.queue }) tail
    else
      setFirstRunningJob tail

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
  Just expression -> Just plot.drawCommands
  Nothing -> Nothing

foldDrawCommands :: State -> DrawCommand Unit
foldDrawCommands state = fold $ [ state.clearPlot ] <> mapMaybe toMaybeDrawCommand state.plots

xyBounds :: Rational -> Rational -> Rational -> Rational -> XYBounds
xyBounds xLower xUpper yLower yUpper = { xBounds: { upper: xUpper, lower: xLower }, yBounds: { upper: yUpper, lower: yLower } }

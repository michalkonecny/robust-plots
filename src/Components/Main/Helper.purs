module Components.Main.Helper where

import Prelude
import Components.Canvas (xyBounds)
import Components.Main.Types (ExpressionPlot, State)
import Data.Array (fold, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Plot.Commands (PlotCommand, robustPlot)
import Types (Id, XYBounds)

newPlot :: Int -> ExpressionPlot
newPlot id = { expressionText: "", expression: Nothing, id, drawCommands: pure unit }

updateExpressionPlotInfo :: Expression -> String -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotInfo expression text = _ { expressionText = text, expression = Just expression, drawCommands = pure unit }

updateExpressionPlotCommands :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotCommands commands plot = plot { drawCommands = fold [ plot.drawCommands, commands ] }

alterPlot :: (ExpressionPlot -> ExpressionPlot) -> Id -> Array ExpressionPlot -> Array ExpressionPlot
alterPlot alterF id = map mapper
  where
  mapper :: ExpressionPlot -> ExpressionPlot
  mapper plot = if plot.id == id then alterF plot else plot

initialBounds :: XYBounds
initialBounds = xyBounds (-one) one (-one) one

toMaybePlotCommandWithId :: XYBounds -> (XYBounds -> Expression -> String -> PlotCommand) -> ExpressionPlot -> Maybe (Tuple PlotCommand Id)
toMaybePlotCommandWithId newBounds plotter plot = case plot.expression of
  Just expression -> Just $ Tuple (plotter newBounds expression plot.expressionText) plot.id
  Nothing -> Nothing

robustWithBounds :: XYBounds -> Expression -> String -> PlotCommand
robustWithBounds bounds expression label = robustPlot bounds bounds.xBounds expression label

toMaybeDrawCommand :: ExpressionPlot -> Maybe (DrawCommand Unit)
toMaybeDrawCommand plot = case plot.expression of
  Just expression -> Just plot.drawCommands
  Nothing -> Nothing

foldDrawCommands :: State -> DrawCommand Unit
foldDrawCommands state = fold $ [ state.clearPlot ] <> mapMaybe toMaybeDrawCommand state.plots

module Components.Main.Helper where

import Prelude
import Components.Main.Types (ExpressionPlot, State)
import Data.Array (fold, mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import IntervalArith.Misc (Rational)
import Plot.Commands (PlotCommand, robustPlot)
import Types (Id, XYBounds)

newPlot :: Int -> ExpressionPlot
newPlot id = { expressionText: "", expression: Nothing, id, drawCommands: pure unit }

updateExpressionPlotInfo :: Expression -> String -> DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotInfo expression text commands = _ { expressionText = text, expression = Just expression, drawCommands = commands }

updateExpressionPlotCommands :: DrawCommand Unit -> ExpressionPlot -> ExpressionPlot
updateExpressionPlotCommands commands plot = plot { drawCommands = fold [ plot.drawCommands, commands ] }

alterPlot :: (ExpressionPlot -> ExpressionPlot) -> Id -> Array ExpressionPlot -> Array ExpressionPlot
alterPlot alterF id = map mapper
  where
  mapper :: ExpressionPlot -> ExpressionPlot
  mapper plot = if plot.id == id then alterF plot else plot

initialBounds :: XYBounds
initialBounds = xyBounds (-one) one (-one) one

toMaybePlotCommandWithId :: XYBounds -> ExpressionPlot -> Maybe (Tuple PlotCommand Id)
toMaybePlotCommandWithId newBounds plot = case plot.expression of
  Just expression -> Just $ Tuple (robustPlot newBounds expression plot.expressionText) plot.id
  Nothing -> Nothing

toMaybeDrawCommand :: ExpressionPlot -> Maybe (DrawCommand Unit)
toMaybeDrawCommand plot = case plot.expression of
  Just expression -> Just plot.drawCommands
  Nothing -> Nothing

foldDrawCommands :: State -> DrawCommand Unit
foldDrawCommands state = fold $ [ state.clearPlot ] <> mapMaybe toMaybeDrawCommand state.plots

xyBounds :: Rational -> Rational -> Rational -> Rational -> XYBounds
xyBounds xLower xUpper yLower yUpper = { xBounds: { upper: xUpper, lower: xLower }, yBounds: { upper: yUpper, lower: yLower } }

module ViewModels.Expression.Unsafe where

import Prelude
import Expression.Syntax (Expression)
import Misc.ExpectAff (ExpectAff, mapExpectAff)
import Types (Size, XYBounds)
import ViewModels.Expression (ExpressionViewModel(..))
import ViewModels.Expression.Common (fromPixelAccuracy)
import ViewModels.Expression.Function.Draw (overwriteFunctionExpression)

expressionText :: ExpressionViewModel -> String
expressionText (Function vm) = vm.expressionText

overwriteExpression :: Expression -> String -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
overwriteExpression expression text autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ overwriteFunctionExpression
        vm
        expression
        text
        autoRobust
        (fromPixelAccuracy size bounds)
        batchSegmentCount
        size
        bounds

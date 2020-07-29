module ViewModels.Expression.Unsafe where

import Prelude
import Effect.Exception.Unsafe (unsafeThrow)
import Expression.Syntax (Expression)
import Misc.ExpectAff (ExpectAff, mapExpectAff)
import Types (Size, XYBounds, Bounds)
import ViewModels.Expression (ExpressionViewModel(..))
import ViewModels.Expression.Common (fromPixelAccuracy)
import ViewModels.Expression.Function.Draw (overwriteFunctionExpression) as F
import ViewModels.Expression.Parametric (ParametricExpression, ParametricExpressionText)
import ViewModels.Expression.Parametric.Draw (overwriteParametricExpression, overwriteParametricDomain) as P

overwriteFunctionExpression :: Expression -> String -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
overwriteFunctionExpression expression text autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ F.overwriteFunctionExpression
        vm
        expression
        text
        autoRobust
        (fromPixelAccuracy size bounds)
        batchSegmentCount
        size
        bounds

overwriteFunctionExpression _ _ _ _ _ _ _ = unsafeThrow "Unsupported operation: 'overwriteFunctionExpression'"

overwriteParametricExpression :: ParametricExpression -> ParametricExpressionText -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
overwriteParametricExpression expressions text autoRobust batchSegmentCount size bounds (Parametric vm) =
  mapExpectAff Parametric
    $ P.overwriteParametricExpression
        vm
        expressions
        text
        autoRobust
        (fromPixelAccuracy size bounds)
        batchSegmentCount
        size
        bounds

overwriteParametricExpression _ _ _ _ _ _ _ = unsafeThrow "Unsupported operation: 'overwriteParametricExpression'"

overwriteParametricDomain :: Bounds -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
overwriteParametricDomain domain autoRobust batchSegmentCount size bounds (Parametric vm) =
  mapExpectAff Parametric
    $ P.overwriteParametricDomain
        vm
        domain
        autoRobust
        (fromPixelAccuracy size bounds)
        batchSegmentCount
        size
        bounds

overwriteParametricDomain _ _ _ _ _ _ = unsafeThrow "Unsupported operation: 'overwriteParametricDomain'"

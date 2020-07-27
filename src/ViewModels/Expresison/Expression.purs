module ViewModels.Expression where

import Prelude
import Control.Parallel (parSequence)
import Data.Either (Either(..))
import Expression.Syntax (Expression)
import Misc.Array (alterWhere)
import Misc.ExpectAff (ExpectAff, ExpectArrayAff, mapExpectAff)
import Plot.JobBatcher (JobQueue)
import Types (Id, XYBounds, Size)
import ViewModels.Expression.Common (AccuracyCalculator)
import ViewModels.Expression.Function (FunctionViewModel, drawRobustOnlyFunction, drawRoughAndRobustFunction, enqueueFunctionExpression, overwiteFunctionAccuracy, overwriteFunctionExpression)

data ExpressionViewModel
  = Function FunctionViewModel

enqueueExpression :: ExpressionViewModel -> Number -> Int -> XYBounds -> ExpectAff JobQueue
enqueueExpression (Function vm) accuracyTarget batchSegmentCount bounds =
  enqueueFunctionExpression
    vm
    accuracyTarget
    batchSegmentCount
    bounds

expressionId :: ExpressionViewModel -> Id
expressionId (Function vm) = vm.id

overwiteAccuracy :: ExpressionViewModel -> Number -> Int -> XYBounds -> ExpectAff ExpressionViewModel
overwiteAccuracy (Function vm) accuracyTarget batchSegmentCount bounds =
  mapExpectAff Function
    $ overwiteFunctionAccuracy
        vm
        accuracyTarget
        batchSegmentCount
        bounds

overwriteExpression :: ExpressionViewModel -> Expression -> String -> Boolean -> AccuracyCalculator -> Int -> Size -> XYBounds -> ExpectAff ExpressionViewModel
overwriteExpression (Function vm) expression text autoRobust toDomainAccuracy batchSegmentCount size bounds =
  mapExpectAff Function
    $ overwriteFunctionExpression
        vm
        expression
        text
        autoRobust
        toDomainAccuracy
        batchSegmentCount
        size
        bounds

drawRoughAndRobust :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRoughAndRobust toDomainAccuracy autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ drawRoughAndRobustFunction
        toDomainAccuracy
        autoRobust
        batchSegmentCount
        size
        bounds
        vm

drawRobustOnly :: AccuracyCalculator -> Boolean -> Int -> Size -> XYBounds -> ExpressionViewModel -> ExpectAff ExpressionViewModel
drawRobustOnly toDomainAccuracy autoRobust batchSegmentCount size bounds (Function vm) =
  mapExpectAff Function
    $ drawRobustOnlyFunction
        toDomainAccuracy
        autoRobust
        batchSegmentCount
        size
        bounds
        vm

alterExpression :: (ExpressionViewModel -> ExpressionViewModel) -> Id -> Array ExpressionViewModel -> Array ExpressionViewModel
alterExpression alterF id = alterWhere (\e -> id == expressionId e) alterF

alterExpressionAsync :: (ExpressionViewModel -> ExpectAff ExpressionViewModel) -> Id -> Array ExpressionViewModel -> ExpectArrayAff ExpressionViewModel
alterExpressionAsync alterF id = parSequence <<< map (\e -> if id == expressionId e then alterF e else pure $ Right e)

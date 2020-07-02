module Plot.PlotEvaluator where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Differentiator (differentiate, secondDifferentiate)
import Expression.Evaluator (evaluate, roughEvaluate)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx)

evaluateA :: Expression -> Approx -> Maybe Approx
evaluateA expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> Nothing
    Right v -> Just v

evaluateN :: Expression -> Number -> Maybe Number
evaluateN expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case roughEvaluate variableMap expression of
    Left _ -> Nothing
    Right v -> Just v

type ExpressionEvaluator a
  = { f :: a -> Maybe a
    , f' :: a -> Maybe a
    , f'' :: a -> Maybe a
    }

buildExpressionEvaluator :: forall a. Expression -> (Expression -> a -> Maybe a) -> ExpressionEvaluator a
buildExpressionEvaluator expression evaluator = { f, f', f'' }
  where
  f = evaluator expression

  f' = (evaluator <<< simplify <<< differentiate) expression

  f'' = (evaluator <<< simplify <<< secondDifferentiate) expression

numberExpressionEvaluator :: Expression -> ExpressionEvaluator Number
numberExpressionEvaluator expression = buildExpressionEvaluator expression evaluateN

approxExpressionEvaluator :: Expression -> ExpressionEvaluator Approx
approxExpressionEvaluator expression = buildExpressionEvaluator expression evaluateA

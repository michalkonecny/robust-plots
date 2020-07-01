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

evaluateWithX :: Expression -> Approx -> Maybe Approx
evaluateWithX expression x = value
  where
  variableMap = [ Tuple "x" x ]

  value = case evaluate variableMap expression of
    Left _ -> Nothing
    Right v -> Just v

evaluateNumberWithX :: Expression -> Number -> Maybe Number
evaluateNumberWithX expression x = value
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

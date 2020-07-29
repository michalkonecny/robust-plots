module Plot.Parametric where

import Data.Maybe (Maybe(..))
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2, ValueAndDerivative)
import Expression.Syntax (Expression)

type ValueAndDerivativePair2 a
  = { x :: ValueAndDerivative2 a, y :: ValueAndDerivative2 a }

type ValueAndDerivativePair a
  = { x :: ValueAndDerivative a, y :: ValueAndDerivative a }

evaluateParametric :: forall a. (Expression -> Maybe (ValueAndDerivative a)) -> Expression -> Expression -> Maybe (ValueAndDerivativePair a)
evaluateParametric evaluator xExpression yExpression = case evaluator xExpression, evaluator yExpression of
  Just x, Just y -> Just { x, y }
  _, _ -> Nothing

evaluateParametric2 :: forall a. (Expression -> Maybe (ValueAndDerivative2 a)) -> Expression -> Expression -> Maybe (ValueAndDerivativePair2 a)
evaluateParametric2 evaluator xExpression yExpression = case evaluator xExpression, evaluator yExpression of
  Just x, Just y -> Just { x, y }
  _, _ -> Nothing

toX :: forall a v r. { x :: v a, y :: v a | r } -> v a
toX = _.x

toY :: forall a v r. { x :: v a, y :: v a | r } -> v a
toY = _.y

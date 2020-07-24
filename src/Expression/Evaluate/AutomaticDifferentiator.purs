module Expression.Evaluate.AutomaticDifferentiator where

import Prelude hiding (min, max, bottom)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Ratio ((%))
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, evaluationError, unknownValue)
import Expression.Evaluate.OperatorClasses (class CanEvaluate, abs, bottom, branchBySign, cos, eWithSample, exp, fromRationalWithSample, intPower, isZero, log, max, min, piWithSample, power, sin, sqrt, tan, union)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.VariableMap (VariableMap, lookup)
import IntervalArith.Misc (big, two, (^))

type ValueAndDerivative a
  = { value :: a, derivative :: a }

evaluateDerivative :: forall a. CanEvaluate a => VariableMap (ValueAndDerivative a) -> Expression -> Expect (ValueAndDerivative a)
evaluateDerivative variableMap expr = do
  sample <- getSample variableMap
  evaluateDerivativeWithSample variableMap sample expr

evaluateDerivativeWithSample :: forall a. CanEvaluate a => VariableMap (ValueAndDerivative a) -> a -> Expression -> Expect (ValueAndDerivative a)
evaluateDerivativeWithSample variableMap sample = evaluate
  where
  evaluate = case _ of
    ExpressionLiteral valueR -> do
      value <- fromRationalWithSample sample valueR
      pure { value, derivative: zero }
    ExpressionVariable "pi" -> pure { value: piWithSample sample, derivative: zero }
    ExpressionVariable "e" -> pure { value: eWithSample sample, derivative: zero }
    ExpressionVariable name -> case lookup variableMap name of
      Just valueAndDerivative -> pure valueAndDerivative
      _ -> unknownValue name
    ExpressionUnary operation subExpression -> evaluateUnary operation subExpression
    ExpressionBinary operation leftExpression rightExpression -> evaluateBinary operation leftExpression rightExpression
    ExpressionIntPower leftExpression n -> evaluateIntPower leftExpression n
    ExpressionLet name expression parentExpression -> do
      expressionValueAndDerivative <- evaluate expression
      evaluateDerivative ([ (Tuple name expressionValueAndDerivative) ] <> variableMap) parentExpression

  evaluateBinary operation leftExpression rightExpression = do
    { value: u, derivative: u' } <- evaluate leftExpression
    { value: v, derivative: v' } <- evaluate rightExpression
    case operation of
      Plus -> pure { value: u + v, derivative: u' + v' }
      Minus -> pure { value: u - v, derivative: u' - v' }
      Times -> pure { value: u * v, derivative: u * v' + u' * v }
      Divide -> pure { value: u / v, derivative: (u' * v - u * v') / (v ^ 2) }
      -- (g^f)' = g^(f-1) * ((f*g')+(g*f'*log(g)))
      -- source: https://www.wolframalpha.com/input/?i=(f%5E(g))%27
      Power
        | isZero v' -> do
          uPowV <- u `power` v
          uPowVminus1 <- u `power` (v - one)
          pure { value: uPowV, derivative: uPowVminus1 * v * u' }
      Power -> do
        uPowV <- u `power` v
        uPowVminus1 <- u `power` (v - one)
        logU <- log u
        pure { value: uPowV, derivative: uPowVminus1 * (v * u' + u * v' * logU) }
      Min ->
        pure
          { value: min u v
          , derivative:
              branchBySign (u - v)
                { positive: v', zero: union u' v', unknown: union u' v', negative: u' }
          }
      Max ->
        pure
          { value: max u v
          , derivative:
              branchBySign (v - u)
                { positive: v', zero: union u' v', unknown: union u' v', negative: u' }
          }

  evaluateIntPower leftExpression n = do
    { value: u, derivative: u' } <- evaluate leftExpression
    v <- fromRationalWithSample u ((big n) % one)
    let
      uPowV = u `intPower` n

      uPowVminus1 = u `intPower` (n - 1)

      t = (v * u')
    pure
      { value: uPowV
      , derivative: uPowVminus1 * t
      }

  evaluateUnary operation subExpression = do
    { value: u, derivative: u' } <- evaluate subExpression
    case operation of
      Neg -> pure { value: -u, derivative: -u' }
      Abs ->
        pure
          { value: abs u
          , derivative:
              branchBySign u
                { positive: u', zero: union u' (-u'), unknown: union u' (-u'), negative: -u' }
          }
      Sqrt -> do
        sqrtU <- sqrt u
        pure { value: sqrtU, derivative: u' / (two * sqrtU) }
      -- (sqrt(f))' = f'/(2*sqrt(f))
      Exp ->
        let
          expU = exp u
        in
          pure { value: expU, derivative: u' * expU }
      Log -> do
        logU <- log u
        pure { value: logU, derivative: u' / u }
      Sine -> pure { value: sin u, derivative: u' * (cos u) }
      Cosine -> pure { value: cos u, derivative: -u' * (sin u) }
      -- tan(f)' = f' * (1 + tan^2(f)) 
      Tan ->
        let
          tanU = tan u
        in
          pure { value: tanU, derivative: u' * (one + tanU ^ 2) }

type ValueAndDerivative2 a
  = { value :: a, derivative :: a, derivative2 :: a }

evaluateDerivative2 :: forall a. Show a => CanEvaluate a => VariableMap (ValueAndDerivative2 a) -> Expression -> Expect (ValueAndDerivative2 a)
evaluateDerivative2 variableMap expr = do
  sample <- getSample variableMap
  evaluateDerivative2WithSample variableMap sample expr

evaluateDerivative2WithSample :: forall a. Show a => CanEvaluate a => VariableMap (ValueAndDerivative2 a) -> a -> Expression -> Expect (ValueAndDerivative2 a)
evaluateDerivative2WithSample variableMap sample = evaluate
  where
  evaluate = case _ of
    ExpressionLiteral valueR -> do
      value <- fromRationalWithSample sample valueR
      pure { value, derivative: zero, derivative2: zero }
    ExpressionVariable "pi" -> pure { value: piWithSample sample, derivative: zero, derivative2: zero }
    ExpressionVariable "e" -> pure { value: eWithSample sample, derivative: zero, derivative2: zero }
    ExpressionVariable name -> case lookup variableMap name of
      Just valueAndDerivative2 -> pure valueAndDerivative2
      _ -> unknownValue name
    ExpressionUnary operation subExpression -> evaluateUnary operation subExpression
    ExpressionBinary operation leftExpression rightExpression -> evaluateBinary operation leftExpression rightExpression
    ExpressionIntPower leftExpression n -> evaluateIntPower leftExpression n
    ExpressionLet name expression parentExpression -> do
      expressionValueAndDerivative2 <- evaluate expression
      evaluateDerivative2 ([ (Tuple name expressionValueAndDerivative2) ] <> variableMap) parentExpression

  evaluateBinary operation leftExpression rightExpression = do
    { value: u, derivative: u', derivative2: u'' } <- evaluate leftExpression
    { value: v, derivative: v', derivative2: v'' } <- evaluate rightExpression
    case operation of
      Plus -> pure { value: u + v, derivative: u' + v', derivative2: u'' + v'' }
      Minus -> pure { value: u - v, derivative: u' - v', derivative2: u'' - v'' }
      Times ->
        pure
          { value: u * v
          , derivative: u * v' + u' * v
          , derivative2: u * v'' + two * (u' * v') + u'' * v
          }
      Divide ->
        pure
          { value: u / v
          , derivative: (u' * v - u * v') / (v ^ 2)
          , derivative2: u'' / v - (two * u' * v' + u * v'') / (v ^ 2) + two * u * v' ^ 2 / (v ^ 3)
          }
      -- (g^f)' = g^(f-1) * ((f*g')+(g*f'*log(g)))
      -- source: https://www.wolframalpha.com/input/?i=(f%5E(g))%27
      Power
        | isZero v' && isZero v'' -> do
          uPowV <- u `power` v
          uPowVminus1 <- u `power` (v - one)
          uPowVminus2 <- u `power` (v - two)
          let
            t = (v * u')
          pure
            { value: uPowV
            , derivative: uPowVminus1 * t
            , derivative2: uPowVminus2 * (t ^ 2 + (v * u'') * u - v * u' ^ 2)
            }
      Power -> do
        uPowV <- u `power` v
        logU <- log u
        let
          t = (v * u') / u + logU * v'
        pure
          { value: uPowV
          , derivative: uPowV * t
          , derivative2: uPowV * (t ^ 2 + (v * u'' + two * u' * v') / u - v * u' ^ 2 / u ^ 2 + logU * v'')
          }
      Min ->
        let
          ddUnion = if isZero (u' - v') then union u'' v'' else bottom
        in
          pure
            { value: min u v
            , derivative:
                branchBySign (u - v)
                  { positive: v', zero: union u' v', unknown: union u' v', negative: u' }
            , derivative2:
                branchBySign (u - v)
                  { positive: v'', zero: ddUnion, unknown: ddUnion, negative: u'' }
            }
      Max ->
        let
          ddUnion = if isZero (u' - v') then union u'' v'' else bottom
        in
          pure
            { value: max u v
            , derivative:
                branchBySign (v - u)
                  { positive: v', zero: union u' v', unknown: union u' v', negative: u' }
            , derivative2:
                branchBySign (v - u)
                  { positive: v'', zero: ddUnion, unknown: ddUnion, negative: u'' }
            }

  evaluateIntPower leftExpression n = do
    { value: u, derivative: u', derivative2: u'' } <- evaluate leftExpression
    v <- fromRationalWithSample u ((big n) % one)
    let
      uPowV = u `intPower` n

      uPowVminus1 = u `intPower` (n - 1)

      uPowVminus2 = u `intPower` (n - 2)

      t = (v * u')
    pure
      { value: uPowV
      , derivative: uPowVminus1 * t
      , derivative2: uPowVminus2 * (t ^ 2 + (v * u'') * u - v * u' ^ 2)
      }

  evaluateUnary operation subExpression = do
    { value: u, derivative: u', derivative2: u'' } <- evaluate subExpression
    case operation of
      Neg -> pure { value: -u, derivative: -u', derivative2: -u'' }
      Abs ->
        let
          ddUnion = if isZero u' then union u'' (-u'') else bottom
        in
          pure
            { value: abs u
            , derivative:
                branchBySign u
                  { positive: u', zero: union u' (-u'), unknown: union u' (-u'), negative: -u' }
            , derivative2:
                branchBySign u
                  { positive: u'', zero: ddUnion, unknown: ddUnion, negative: -u'' }
            }
      Sqrt -> do
        sqrtU <- sqrt u
        pure
          { value: sqrtU
          , derivative: u' / (two * sqrtU)
          , derivative2: (two * u * u'' - u' ^ 2) / (two * u * two * sqrtU)
          }
      -- (sqrt(f))' = f'/(2*sqrt(f))
      Exp ->
        let
          expU = exp u
        in
          pure { value: expU, derivative: u' * expU, derivative2: expU * (u'' + u' ^ 2) }
      Log -> do
        logU <- log u
        pure { value: logU, derivative: u' / u, derivative2: (u * u'' - u' ^ 2) / u ^ 2 }
      Sine ->
        let
          sinU = sin u

          cosU = cos u
        in
          pure { value: sinU, derivative: u' * cosU, derivative2: u'' * cosU - (u' ^ 2) * sinU }
      Cosine ->
        let
          sinU = sin u

          cosU = cos u
        in
          pure { value: cosU, derivative: -u' * sinU, derivative2: -u'' * sinU - (u' ^ 2) * cosU }
      -- tan(f)' = f' * (1 + tan^2(f)) 
      Tan ->
        let
          tanU = tan u

          cos2U = cos (two * u)
        in
          pure
            { value: tanU
            , derivative: u' * (one + tanU ^ 2)
            , derivative2: two * (u'' + two * u' ^ 2 * tanU) / (cos2U + one)
            }

getSample :: forall a b. VariableMap { value :: a | b } -> Expect a
getSample variableMap = case head variableMap of
  Just (Tuple _ s) -> pure $ s.value
  Nothing -> evaluationError "At least one variable has to be specified"

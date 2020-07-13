module Expression.Evaluator.AutomaticDifferentiator where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Data.Tuple (Tuple(..))
import Expression.Error (Expect, evaluationError, unknownValue)
import Expression.Syntax (BinaryOperation(..), Expression(..), UnaryOperation(..))
import Expression.VariableMap (VariableMap, lookup)
import IntervalArith.Misc (Rational, rationalToNumber, two, (^))
import Math as Math

class HasRational a where
  fromRational :: Rational -> Expect a

class HasIsZero a where
  isZero :: a -> Boolean

class HasSqrt a where
  sqrt :: a -> Expect a

class HasPower a where
  power :: a -> a -> Expect a

class HasSinCos a where
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a

class HasExpLog a where
  exp :: a -> a
  log :: a -> Expect a

instance numberHasRational :: HasRational Number where
  fromRational = checkNumber "illegal literal" <<< rationalToNumber

instance numberHasIsZero :: HasIsZero Number where
  isZero = (_ == 0.0)

instance numberHasSqrt :: HasSqrt Number where
  sqrt = checkNumber "sqrt: parameter out of range" <<< Math.sqrt

instance numberHasPower :: HasPower Number where
  power a e = checkNumber "sqrt: parameter out of range" $ Math.exp (e * (Math.log a))

instance numberHasSinCos :: HasSinCos Number where
  sin = Math.sin
  cos = Math.cos
  tan = Math.tan

instance numberHasExpLog :: HasExpLog Number where
  exp = Math.exp
  log = checkNumber "log: parameter out of range" <<< Math.log

checkNumber :: String -> Number -> Expect Number
checkNumber message n
  | isNaN n = evaluationError message
  | otherwise = pure n

type ValueAndDerivative a
  = { value :: a, derivative :: a }

evaluateDerivative ::
  forall a.
  (Field a) =>
  (HasRational a) =>
  (HasIsZero a) =>
  (HasSqrt a) =>
  (HasPower a) =>
  (HasSinCos a) =>
  (HasExpLog a) =>
  (VariableMap (ValueAndDerivative a)) ->
  Expression ->
  Expect (ValueAndDerivative a)
evaluateDerivative variableMap = eval
  where
  eval = case _ of
    ExpressionLiteral valueR -> do
      value <- fromRational valueR
      pure { value, derivative: zero }
    ExpressionVariable name -> case lookup variableMap name of
      Just valueAndDerivative -> pure valueAndDerivative
      _ -> unknownValue name
    ExpressionBinary operation leftExpression rightExpression -> evalBinary operation leftExpression rightExpression
    ExpressionUnary operation subExpression -> evalUnary operation subExpression
    ExpressionLet name expression parentExpression -> do
      expressionValueAndDerivative <- eval expression
      evaluateDerivative ([ (Tuple name expressionValueAndDerivative) ] <> variableMap) parentExpression

  evalBinary operation leftExpression rightExpression = do
    { value: u, derivative: u' } <- eval leftExpression
    { value: v, derivative: v' } <- eval rightExpression
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

  evalUnary operation subExpression = do
    { value: u, derivative: u' } <- eval subExpression
    case operation of
      Neg -> pure { value: -u, derivative: -u' }
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

evaluateDerivative2 ::
  forall a.
  (Field a) =>
  (HasRational a) =>
  (HasIsZero a) =>
  (HasSqrt a) =>
  (HasPower a) =>
  (HasSinCos a) =>
  (HasExpLog a) =>
  (VariableMap (ValueAndDerivative2 a)) ->
  Expression ->
  Expect (ValueAndDerivative2 a)
evaluateDerivative2 variableMap = eval
  where
  eval = case _ of
    ExpressionLiteral valueR -> do
      value <- fromRational valueR
      pure { value, derivative: zero, derivative2: zero }
    ExpressionVariable name -> case lookup variableMap name of
      Just valueAndDerivative2 -> pure valueAndDerivative2
      _ -> unknownValue name
    ExpressionBinary operation leftExpression rightExpression -> evalBinary operation leftExpression rightExpression
    ExpressionUnary operation subExpression -> evalUnary operation subExpression
    ExpressionLet name expression parentExpression -> do
      expressionValueAndDerivative2 <- eval expression
      evaluateDerivative2 ([ (Tuple name expressionValueAndDerivative2) ] <> variableMap) parentExpression

  evalBinary operation leftExpression rightExpression = do
    { value: u, derivative: u', derivative2: u'' } <- eval leftExpression
    { value: v, derivative: v', derivative2: v'' } <- eval rightExpression
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
          , derivative2: (u'' * v ^ 2 - v * (two * u' * v' + u * v'') + two * u * v' ^ 2) / (v ^ 3)
          }
      -- (g^f)' = g^(f-1) * ((f*g')+(g*f'*log(g)))
      -- source: https://www.wolframalpha.com/input/?i=(f%5E(g))%27
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

  evalUnary operation subExpression = do
    { value: u, derivative: u', derivative2: u'' } <- eval subExpression
    case operation of
      Neg -> pure { value: -u, derivative: -u', derivative2: -u'' }
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

t2 :: Expression
t2 = ExpressionBinary Power (ExpressionVariable "x") (ExpressionLiteral two)

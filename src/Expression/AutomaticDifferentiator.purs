module Expression.AutomaticDifferentiator where

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

module Expression.EvaluationResult where

import Prelude
import IntervalArith.Misc (Rational, rationalToNumber, toRational)
import Math as Math

data EvaluateResult
  = Number Number
  | Rational Rational

instance semiringEvaluateResult :: Semiring EvaluateResult where
  zero = Rational $ toRational 0
  one = Rational $ toRational 1
  add (Rational a) (Rational b) = Rational $ a + b
  add (Number a) (Rational b) = Number $ a + (rationalToNumber b)
  add (Rational a) (Number b) = Number $ (rationalToNumber a) + b
  add (Number a) (Number b) = Number $ a + b
  mul (Rational a) (Rational b) = Rational $ a * b
  mul (Number a) (Rational b) = Number $ a * (rationalToNumber b)
  mul (Rational a) (Number b) = Number $ (rationalToNumber a) * b
  mul (Number a) (Number b) = Number $ a * b

instance euclideanRingEvaluateResult :: EuclideanRing EvaluateResult where
  div (Rational a) (Rational b) = Rational $ a / b
  div (Number a) (Rational b) = Number $ a / (rationalToNumber b)
  div (Rational a) (Number b) = Number $ (rationalToNumber a) / b
  div (Number a) (Number b) = Number $ a / b
  degree _ = 1
  mod (Rational a) (Rational b) = Rational $ mod a b
  mod (Number a) (Rational b) = Number $ mod a (rationalToNumber b)
  mod (Rational a) (Number b) = Number $ mod (rationalToNumber a) b
  mod (Number a) (Number b) = Number $ mod a b

instance commutativeRingDyadic :: CommutativeRing EvaluateResult

instance ringDyadic :: Ring EvaluateResult where
  sub (Rational a) (Rational b) = Rational $ a - b
  sub (Number a) (Rational b) = Number $ a - (rationalToNumber b)
  sub (Rational a) (Number b) = Number $ (rationalToNumber a) - b
  sub (Number a) (Number b) = Number $ a - b

exp :: EvaluateResult -> EvaluateResult
exp (Rational value) = Number $ Math.exp $ rationalToNumber value

exp (Number value) = Number $ Math.exp value

log :: EvaluateResult -> EvaluateResult
log (Rational value) = Number $ Math.log $ rationalToNumber value

log (Number value) = Number $ Math.log value

sqrt :: EvaluateResult -> EvaluateResult
sqrt (Rational value) = Number $ Math.sqrt $ rationalToNumber value

sqrt (Number value) = Number $ Math.sqrt value

pow :: EvaluateResult -> EvaluateResult -> EvaluateResult
pow (Rational a) (Rational b) = Number $ Math.pow (rationalToNumber a) (rationalToNumber b)

pow (Number a) (Rational b) = Number $ Math.pow a (rationalToNumber b)

pow (Rational a) (Number b) = Number $ Math.pow (rationalToNumber a) b

pow (Number a) (Number b) = Number $ Math.pow a b

sin :: EvaluateResult -> EvaluateResult
sin (Rational value) = Number $ Math.sin $ rationalToNumber value

sin (Number value) = Number $ Math.sin value

tan :: EvaluateResult -> EvaluateResult
tan (Rational value) = Number $ Math.tan $ rationalToNumber value

tan (Number value) = Number $ Math.tan value

cos :: EvaluateResult -> EvaluateResult
cos (Rational value) = Number $ Math.cos $ rationalToNumber value

cos (Number value) = Number $ Math.cos value

e :: EvaluateResult
e = Number Math.e

pi :: EvaluateResult
pi = Number Math.pi

toNumber :: EvaluateResult -> Number
toNumber (Rational value) = rationalToNumber value

toNumber (Number value) = value

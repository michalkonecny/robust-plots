module Expression.Evaluate.OperatorClasses where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Expression.Error (Expect, evaluationError)
import IntervalArith.Approx (Approx(..), fromRationalPrec, mBound)
import IntervalArith.Approx.ExpLog (expA, logA, powA)
import IntervalArith.Approx.SinCos (cosA, sinA, tanA)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Misc (Rational, rationalToNumber, (^^))
import Math as Math

class HasRational a where
  fromRational :: a -> Rational -> Expect a

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

-- | Composite class that defines all the instances a type must have to be evaluated and derived.
class (Field a, HasRational a, HasIsZero a, HasSqrt a, HasPower a, HasSinCos a, HasExpLog a) <= CanEvaluate a

-- Number instances:

instance numberHasRational :: HasRational Number where
  fromRational _sample = checkNumber "illegal literal" <<< rationalToNumber

instance numberHasIsZero :: HasIsZero Number where
  isZero = (_ == 0.0)

instance numberHasSqrt :: HasSqrt Number where
  sqrt = checkNumber "sqrt: parameter out of range" <<< Math.sqrt

instance numberHasPower :: HasPower Number where
  power a e = 
    checkNumber "power: parameter out of range" $ 
    if e == Int.toNumber eInt then
      a ^^ eInt
    else
      Math.exp (e * (Math.log a))
    where
    eInt = Int.round e

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

instance numberCanEvaluate :: CanEvaluate Number

-- Approx instances:

instance approxHasRational :: HasRational Approx where
  fromRational sample = pure <<< fromRationalPrec (mBound sample)

instance approxHasIsZero :: HasIsZero Approx where
  isZero (Approx _ m e _) = (m == zero) && (e == zero)
  isZero _ = false

instance approxHasSqrt :: HasSqrt Approx where
  sqrt = checkApprox "sqrt: parameter out of range" <<< sqrtA

instance approxHasPower :: HasPower Approx where
  power a e = 
    checkApprox "power: parameter out of range" $ powA a e

instance approxHasSinCos :: HasSinCos Approx where
  sin = sinA
  cos = cosA
  tan = tanA

instance approxHasExpLog :: HasExpLog Approx where
  exp = expA
  log = checkApprox "log: parameter out of range" <<< logA

checkApprox :: String -> Maybe Approx -> Expect Approx
checkApprox message = case _ of
  Just a -> pure a
  _ -> evaluationError message

instance approxCanEvaluate :: CanEvaluate Approx

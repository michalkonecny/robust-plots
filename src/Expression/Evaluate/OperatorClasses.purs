module Expression.Evaluate.OperatorClasses where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Data.Ord (signum)
import Data.Ord as Ord
import Expression.Error (Expect, evaluationError)
import IntervalArith.Approx (Approx(..), fromRationalPrec, mBound, unionA)
import IntervalArith.Approx.ExpLog (expA, logA, powA)
import IntervalArith.Approx.NumOrder (absA, maxA, minA, (!<!))
import IntervalArith.Approx.SinCos (cosA, sinA, tanA)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Misc (Rational, rationalToNumber, (^^))
import Math as Math

class HasRational a where
  fromRational :: a -> Rational -> Expect a

class HasAbs a where
  abs :: a -> a

class HasMinMax a where
  min :: a -> a -> a
  max :: a -> a -> a

class HasUnion a where
  union :: a -> a -> a

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

class HasSign a where
  sign :: a -> Maybe Int

type SignBranches a = { positive :: a, zero :: a, negative :: a, unknown :: a }

branchBySign :: forall a. HasSign a => a -> SignBranches a -> a
branchBySign a branches =
  case sign a of
    Just s 
      | s > 0 -> branches.positive
      | s == 0 -> branches.zero
      | s < 0 -> branches.negative
    _ -> branches.unknown

-- | Composite class that defines all the instances a type must have to be evaluated and derived.
class (Field a, HasRational a, HasAbs a, HasMinMax a, HasUnion a, HasSqrt a, HasPower a, HasSinCos a, HasExpLog a, HasSign a) <= CanEvaluate a

-- Number instances:
instance numberHasRational :: HasRational Number where
  fromRational _sample = checkNumber "illegal literal" <<< rationalToNumber

instance numberHasAbs :: HasAbs Number where
  abs = Ord.abs

instance numberHasMinMax :: HasMinMax Number where
  min = Ord.min
  max = Ord.max

instance numberHasUnion :: HasUnion Number where
  union a b = (a+b)/2.0

instance numberHasSqrt :: HasSqrt Number where
  sqrt = checkNumber "sqrt: parameter out of range" <<< Math.sqrt

instance numberHasPower :: HasPower Number where
  power a e =
    checkNumber "power: parameter out of range"
      $ if e == Int.toNumber eInt then
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

instance numberHasSign :: HasSign Number where
  sign = Just <<< signum <<< Int.round

instance numberCanEvaluate :: CanEvaluate Number

-- Approx instances:
instance approxHasRational :: HasRational Approx where
  fromRational sample = pure <<< fromRationalPrec (mBound sample)

instance approxHasAbs :: HasAbs Approx where
  abs = absA

instance approxHasMinMax :: HasMinMax Approx where
  min = minA
  max = maxA

instance approxHasUnion :: HasUnion Approx where
  union = unionA

instance approxHasSqrt :: HasSqrt Approx where
  sqrt = checkApprox "sqrt: parameter out of range" <<< sqrtA

instance approxHasPower :: HasPower Approx where
  power a e = checkApprox "power: parameter out of range" $ powA a e

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

instance approxHasSign :: HasSign Approx where
  sign a = result
    where
    result
      | a !<! zero = Just (-1)
      | zero !<! a = Just 1
      | isZero_ a = Just 0
      | otherwise = Nothing
    isZero_ (Approx _ m e _) = (m == zero) && (e == zero)
    isZero_ _ = false

isZero :: forall a. HasSign a => a -> Boolean
isZero a = sign a == (Just 0)

instance approxCanEvaluate :: CanEvaluate Approx
module Plot.Segments where

import Prelude
import Data.Array (fromFoldable)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2)
import IntervalArith.Approx (Approx, fromRationalBoundsPrec)
import IntervalArith.Misc (Rational, rationalToNumber, two)

segmentDomain :: Number -> (Number -> Maybe (ValueAndDerivative2 Number)) -> Rational -> Rational -> Array Approx
segmentDomain accuracyTarget evaluator l u = fromFoldable $ segementDomainF 0 l u
  where
  bisect :: Int -> Rational -> Rational -> Rational -> List Approx
  bisect depth lower mid upper = (segementDomainF (depth + one) lower mid) <> (segementDomainF (depth + one) mid upper)

  three = one + two

  segementDomainF :: Int -> Rational -> Rational -> List Approx
  segementDomainF depth lower upper = segments
    where
    x = fromRationalBoundsPrec 50 lower upper

    mid = (lower + upper) / two

    range = upper - lower

    a1 = rationalToNumber $ lower + (range / three)

    a2 = rationalToNumber $ lower + ((range * two) / three)

    segments =
      if depth < 5 then
        bisect depth lower mid upper
      else
        if depth >= 10 then
          singleton x
        else
          segmentBasedOnDerivative depth lower mid upper x (evaluator a1) (evaluator a2) (evaluator (rationalToNumber mid))

  segmentBasedOnDerivative :: Int -> Rational -> Rational -> Rational -> Approx -> Maybe (ValueAndDerivative2 Number) -> Maybe (ValueAndDerivative2 Number) -> Maybe (ValueAndDerivative2 Number) -> List Approx
  segmentBasedOnDerivative depth lower mid upper x (Just a1) (Just a2) (Just b) =
    let
      w = rationalToNumber $ mid - lower

      a = (a1.derivative2 + a2.derivative2) / two

      h = if abs b.derivative > one then abs ((a * w * w) / b.derivative) else abs (a * w * w)
    in
      if h > accuracyTarget then
        bisect depth lower mid upper
      else
        singleton x

  segmentBasedOnDerivative _ _ _ _ x _ _ _ = singleton x

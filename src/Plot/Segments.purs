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
  bisect { depth, lower, mid, upper } =
    (segementDomainF (depth + one) lower mid)
      <> (segementDomainF (depth + one) mid upper)

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
        bisect { depth, lower, mid, upper }
      else
        if depth >= 10 then
          singleton x
        else
          segmentBasedOnDerivative
            { depth, lower, mid, upper }
            x
            (evaluator (rationalToNumber lower))
            (evaluator (rationalToNumber upper))
            (evaluator a1)
            (evaluator a2)
            (evaluator (rationalToNumber mid))

  segmentBasedOnDerivative :: { depth :: Int, lower :: Rational, mid :: Rational, upper :: Rational } -> Approx -> Maybe (ValueAndDerivative2 Number) -> Maybe (ValueAndDerivative2 Number) -> Maybe (ValueAndDerivative2 Number) -> Maybe (ValueAndDerivative2 Number) -> Maybe (ValueAndDerivative2 Number) -> List Approx
  segmentBasedOnDerivative state@{ depth, lower, mid, upper } x _ _ (Just a1) (Just a2) (Just b) =
    let
      w = rationalToNumber $ mid - lower

      a = (a1.derivative2 + a2.derivative2) / two

      h = if abs b.derivative > one then abs ((a * w * w) / b.derivative) else abs (a * w * w)
    in
      if abs (a1.derivative2 - a2.derivative2) * w > 3.0 * accuracyTarget || h > accuracyTarget then
        bisect state
      else
        singleton x

  -- function not defined on either end, assume not defined on the whole segment:
  segmentBasedOnDerivative state x Nothing Nothing Nothing Nothing Nothing = singleton x

  segmentBasedOnDerivative state x _ _ _ _ _ = bisect state

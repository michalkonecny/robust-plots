module Plot.Segments where

import Prelude
import Data.Array (fromFoldable)
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Data.Number as Number
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

  segementDomainF :: Int -> Rational -> Rational -> List Approx
  segementDomainF depth lower upper = segments
    where
    x = fromRationalBoundsPrec 50 lower upper

    mid = (lower + upper) / two

    xL = rationalToNumber lower

    xU = rationalToNumber upper

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
            { fxL: checkFinite $ evaluator xL <#> (_.value)
            , fxU: checkFinite $ evaluator xU <#> (_.value)
            , f'xL: (checkFinite $ evaluator xL <#> (_.derivative))
            , f'xU: (checkFinite $ evaluator xU <#> (_.derivative))
            , f''xL: (checkFinite $ evaluator xL <#> (_.derivative2))
            , f''xU: (checkFinite $ evaluator xU <#> (_.derivative2))
            }

  segmentBasedOnDerivative state@{ depth, lower, mid, upper } x { fxL: Just _, fxU: Just _, f'xL: Just b1, f'xU: Just b2, f''xL: Just a1, f''xU: Just a2 } =
    let
      w = rationalToNumber $ mid - lower

      w2 = w * two

      bUnstable = abs (b1 - b2) * w2 > accuracyTarget

      aUnstable = abs (a1 - a2) * w2 > accuracyTarget
    in
      if bUnstable || aUnstable then
        bisect state
      else
        let
          b = (b1 + b2) / two

          a = (a1 + a2) / two

          h = if abs b > one then abs ((a * w * w) / b) else abs (a * w * w)
        in
          if h > accuracyTarget then
            bisect state
          else
            singleton x

  -- function not defined on either end, assume not defined on the whole segment:
  segmentBasedOnDerivative state x { fxL: Nothing, fxU: Nothing } = singleton x

  segmentBasedOnDerivative state x _ = bisect state

checkFinite :: Maybe Number -> Maybe Number
checkFinite ma@(Just a) = if Number.isFinite a then ma else Nothing

checkFinite ma = ma

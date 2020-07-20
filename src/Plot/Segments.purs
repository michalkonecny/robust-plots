module Plot.Segments where

import Prelude
import Data.Array (fromFoldable)
import Data.List (singleton)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Ord (abs)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2)
import IntervalArith.Approx (Approx, fromRationalBoundsPrec)
import IntervalArith.Misc (Rational, rationalToNumber, two)

segmentDomain :: Number -> (Number -> Maybe (ValueAndDerivative2 Number)) -> Rational -> Rational -> Array Approx
segmentDomain accuracyTarget evaluator l u = fromFoldable $ segmentDomainF { depth: 0, xL: l, xU: u }
  where
  bisect { depth, xL, xM, xU } =
    segmentDomainF { depth: depth + one, xL, xU: xM }
      <> segmentDomainF { depth: depth + one, xL: xM, xU }

  segmentDomainF { depth, xL, xU } = segments
    where
    x = fromRationalBoundsPrec 50 xL xU

    xM = (xL + xU) / two

    evaluatorXL = evaluator (rationalToNumber xL)

    evaluatorXU = evaluator (rationalToNumber xU)

    segments =
      if depth < 5 then
        bisect { depth, xL, xM, xU }
      else
        if depth >= 10 then
          singleton x
        else
          segmentBasedOnDerivative
            { depth, xL, xM, xU }
            x
            { fxL: checkFinite $ evaluatorXL <#> (_.value)
            , fxU: checkFinite $ evaluatorXU <#> (_.value)
            , f'xL: checkFinite $ evaluatorXL <#> (_.derivative)
            , f'xU: checkFinite $ evaluatorXU <#> (_.derivative)
            , f''xL: checkFinite $ evaluatorXL <#> (_.derivative2)
            , f''xU: checkFinite $ evaluatorXU <#> (_.derivative2)
            }

  segmentBasedOnDerivative state@{ depth, xL, xM, xU } x { fxL: Just _, fxU: Just _, f'xL: Just b1, f'xU: Just b2, f''xL: Just a1, f''xU: Just a2 } =
    let
      w = rationalToNumber $ xM - xL

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

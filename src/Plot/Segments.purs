module Plot.Segments where

import Prelude
import Data.Array (fromFoldable, length)
import Data.Int as Int
import Data.List (singleton)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2)
import IntervalArith.Approx (Approx, Precision, fromRationalBoundsPrec, setMB)
import IntervalArith.Misc (Rational, rationalToNumber, two)
import Math (log)
import Misc.Debug (unsafeLog, unsafeSpy)
import Plot.Commands (Depth)

minDepth :: Depth
minDepth = 3

maxDepth :: Depth
maxDepth = 12

minPrecision :: Precision
minPrecision = 10

maxPrecision :: Precision
maxPrecision = 300

segmentDomain ::
  { accuracyTarget :: Number
  , evaluator :: Number -> Maybe (ValueAndDerivative2 Number)
  , l :: Rational
  , u :: Rational
  } ->
  Array (Tuple Int Approx)
segmentDomain { accuracyTarget, evaluator, l, u } = unsafeLog ("segmentDomain: length result = " <> show (length result)) result
  where
  result =
    fromFoldable
      $ segmentDomainF
          { depth: 0
          , xL: l
          , evaluatorXL: evaluator (rationalToNumber l)
          , xU: u
          , evaluatorXU: evaluator (rationalToNumber u)
          }

  xPrecisionBase =
    unsafeSpy "xPrecisionBase"
      $ min maxPrecision
      $ max minPrecision
          (20 - (Int.round $ 5.0 * (log accuracyTarget) / (log 2.0)))

  bisect { depth, xL, evaluatorXL, xM, evaluatorXM, xU, evaluatorXU } =
    segmentDomainF
      { depth: depth + one
      , xL
      , evaluatorXL
      , xU: xM
      , evaluatorXU: evaluatorXM
      }
      <> segmentDomainF
          { depth: depth + one
          , xL: xM
          , evaluatorXL: evaluatorXM
          , xU
          , evaluatorXU
          }

  segmentDomainF { depth, xL, evaluatorXL, xU, evaluatorXU } = segments
    where
    xPrecisionDepth = xPrecisionBase + 2 * depth

    x = setMB xPrecisionDepth $ fromRationalBoundsPrec xPrecisionDepth xL xU

    xM = (xL + xU) / two

    evaluatorXM = evaluator (rationalToNumber xM)

    state = { depth, xL, evaluatorXL, xM, evaluatorXM, xU, evaluatorXU }

    segments =
      if depth < minDepth then
        bisect state
      else
        if depth >= maxDepth then
          singleton (Tuple depth x)
        else
          segmentBasedOnDerivative
            state
            x
            { fxL: checkFinite $ evaluatorXL <#> (_.value)
            , fxU: checkFinite $ evaluatorXU <#> (_.value)
            , f'xL: checkFinite $ evaluatorXL <#> (_.derivative)
            , f'xU: checkFinite $ evaluatorXU <#> (_.derivative)
            , f''xL: checkFinite $ evaluatorXL <#> (_.derivative2)
            , f''xU: checkFinite $ evaluatorXU <#> (_.derivative2)
            }

  segmentBasedOnDerivative state@{ depth, xL, xM } x { fxL: Just _
  , fxU: Just _
  , f'xL: Just b1
  , f'xU: Just b2
  , f''xL: Just a1
  , f''xU: Just a2
  } =
    let
      w = rationalToNumber $ xM - xL
    in
      if w <= accuracyTarget then
        singleton (Tuple depth x)
      else
        let
          w2 = w * two

          bUnstable = abs (b1 - b2) * w > accuracyTarget

          aUnstable = abs (a1 - a2) * w > accuracyTarget
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
                singleton (Tuple depth x)

  -- function not defined on either end, assume not defined on the whole segment:
  segmentBasedOnDerivative state@{ depth } x { fxL: Nothing, fxU: Nothing } = singleton (Tuple depth x)

  segmentBasedOnDerivative state x _ = bisect state

checkFinite :: Maybe Number -> Maybe Number
checkFinite ma@(Just a) = if Number.isFinite a then ma else Nothing

checkFinite ma = ma

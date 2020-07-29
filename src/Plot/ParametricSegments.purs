module Plot.ParametricSegments where

import Prelude
import Data.Array (fromFoldable)
import Data.Int as Int
import Data.List (List, singleton)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Ord (abs)
import Data.Tuple (Tuple(..))
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative2)
import IntervalArith.Approx (Approx, Precision, fromRationalBoundsPrec, setMB)
import IntervalArith.Misc (Rational, rationalToNumber, two)
import Math (log)
import Plot.Commands (Depth)
import Plot.Parametric (ValueAndDerivativePair2)

minDepth :: Depth
minDepth = 3

maxDepth :: Depth
maxDepth = 12

minPrecision :: Precision
minPrecision = 10

maxPrecision :: Precision
maxPrecision = 300

type SegmentState
  = { depth :: Int
    , tL :: Rational
    , tU :: Rational
    , evaluatorTL :: Maybe (ValueAndDerivativePair2 Number)
    , evaluatorTU :: Maybe (ValueAndDerivativePair2 Number)
    }

type SegmentStateWithMidpoint
  = { depth :: Int
    , tL :: Rational
    , tU :: Rational
    , tM :: Rational
    , evaluatorTL :: Maybe (ValueAndDerivativePair2 Number)
    , evaluatorTU :: Maybe (ValueAndDerivativePair2 Number)
    , evaluatorTM :: Maybe (ValueAndDerivativePair2 Number)
    }

type MaybeEvaluated
  = { ftL :: Maybe Number
    , ftU :: Maybe Number
    , f'tL :: Maybe Number
    , f'tU :: Maybe Number
    , f''tL :: Maybe Number
    , f''tU :: Maybe Number
    }

type Derivatives
  = { b1 :: Number
    , b2 :: Number
    , a1 :: Number
    , a2 :: Number
    }

segmentDomain ::
  { accuracyTarget :: Number
  , evaluator :: Number -> Maybe (ValueAndDerivativePair2 Number)
  , l :: Rational
  , u :: Rational
  } ->
  Array (Tuple Int Approx)
segmentDomain { accuracyTarget, evaluator, l, u } = result
  where
  result =
    fromFoldable
      $ segmentDomainF
          { depth: 0
          , tL: l
          , evaluatorTL: evaluator (rationalToNumber l)
          , tU: u
          , evaluatorTU: evaluator (rationalToNumber u)
          }

  xPrecisionBase :: Int
  xPrecisionBase =
    min maxPrecision
      $ max minPrecision
          (40 - (Int.round $ 2.0 * (log accuracyTarget) / (log 2.0)))

  bisect :: SegmentStateWithMidpoint -> List (Tuple Int Approx)
  bisect { depth, tL, evaluatorTL, tM, evaluatorTM, tU, evaluatorTU } = segmentDomainF lowerSegmentParams <> segmentDomainF upperSegmentParams
    where
    lowerSegmentParams =
      { depth: depth + one
      , tL
      , evaluatorTL
      , tU: tM
      , evaluatorTU: evaluatorTM
      }

    upperSegmentParams =
      { depth: depth + one
      , tL: tM
      , evaluatorTL: evaluatorTM
      , tU
      , evaluatorTU
      }

  segmentDomainF :: SegmentState -> List (Tuple Int Approx)
  segmentDomainF { depth, tL, evaluatorTL, tU, evaluatorTU } = segments
    where
    xPrecisionDepth = xPrecisionBase + 2 * depth

    t = setMB xPrecisionDepth $ fromRationalBoundsPrec xPrecisionDepth tL tU

    tM = (tL + tU) / two

    evaluatorTM = evaluator (rationalToNumber tM)

    state = { depth, tL, evaluatorTL, tM, evaluatorTM, tU, evaluatorTU }

    segments =
      if depth < minDepth then
        bisect state
      else
        if depth >= maxDepth then
          singleton (Tuple depth t)
        else
          let
            shouldBisectX = shouldBisect state t (evaluate (_.x) evaluatorTL evaluatorTU)

            shouldBisectY = shouldBisect state t (evaluate (_.y) evaluatorTL evaluatorTU)
          in
            if shouldBisectX || shouldBisectY then
              bisect state
            else
              singleton (Tuple depth t)

  shouldBisect :: SegmentStateWithMidpoint -> Approx -> MaybeEvaluated -> Boolean
  shouldBisect state _ { ftL: Just _, ftU: Just _, f'tL: Just b1, f'tU: Just b2, f''tL: Just a1, f''tU: Just a2 }
    | shouldBisectWithDerivative state { b1, b2, a1, a2 } = true
    | otherwise = false

  -- function not defined on either end, assume not defined on the whole segment:
  shouldBisect _ _ { ftL: Nothing, ftU: Nothing } = false

  shouldBisect _ _ _ = true

  shouldBisectWithDerivative :: SegmentStateWithMidpoint -> Derivatives -> Boolean
  shouldBisectWithDerivative { tL, tM } { b1, b2, a1, a2 } =
    let
      w = rationalToNumber $ tM - tL

      w2 = w * 2.0
    in
      if w2 <= accuracyTarget then
        false
      else
        let
          bUnstable = abs (b1 - b2) * w > accuracyTarget

          aUnstable = abs (a1 - a2) * w > accuracyTarget
        in
          if bUnstable || aUnstable then
            true
          else
            let
              b = (b1 + b2) / two

              a = (a1 + a2) / two

              accuracyEstimate = max (min w2 enclosureParallelogramWidth) (min enclosureBoxHeight enclosureParallelogramHeight)
                where
                enclosureBoxHeight = w2 * b / (1.0 + (abs b))

                enclosureParallelogramHeight = w * w * a

                enclosureParallelogramWidth = enclosureParallelogramHeight / (abs b)
            in
              1.2 * accuracyEstimate > accuracyTarget

checkFinite :: Maybe Number -> Maybe Number
checkFinite ma@(Just a) = if Number.isFinite a then ma else Nothing

checkFinite ma = ma

evaluate :: ((ValueAndDerivativePair2 Number) -> (ValueAndDerivative2 Number)) -> Maybe (ValueAndDerivativePair2 Number) -> Maybe (ValueAndDerivativePair2 Number) -> MaybeEvaluated
evaluate parametricToFunction evaluatorTL evaluatorTU =
  { ftL: checkFinite $ evaluatorTL <#> parametricToFunction <#> (_.value)
  , ftU: checkFinite $ evaluatorTU <#> parametricToFunction <#> (_.value)
  , f'tL: checkFinite $ evaluatorTL <#> parametricToFunction <#> (_.derivative)
  , f'tU: checkFinite $ evaluatorTU <#> parametricToFunction <#> (_.derivative)
  , f''tL: checkFinite $ evaluatorTL <#> parametricToFunction <#> (_.derivative2)
  , f''tU: checkFinite $ evaluatorTU <#> parametricToFunction <#> (_.derivative2)
  }

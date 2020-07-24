module Benchmark.Plotter where

import Prelude
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Components.BoundsInput (unitBounds)
import Data.Array ((..))
import Data.Array.NonEmpty (singleton, toNonEmpty)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, ValueAndDerivative2, evaluateDerivative, evaluateDerivative2)
import Expression.Parser (parse)
import IntervalArith.Approx (Approx)
import IntervalArith.Misc (toRational)
import Plot.RobustPlot (plotEnclosures)
import Plot.RoughPlot (evaluateWithX)
import Plot.Segments (segmentDomain)
import Test.QuickCheck.Gen (elements)
import Types (Polygon, Size, XYBounds)

benchPlot :: Benchmark
benchPlot =
  mkBenchmark
    { slug: "plot"
    , title: "Plotting the '" <> defaultExpression <> "' expression"
    , sizes: (1 .. 5)
    , sizeInterpretation: "Upper bound of accuracy range with 0.1 interval"
    , inputsPerSize: 1
    , gen: \n -> elements $ toNonEmpty $ singleton n
    , functions:
        [ benchFn "plotEnclosures" plotFunc
        ]
    }

-- | Divide the given `Int` by 10 and use it as the accuracy of the plot
plotFunc :: Int -> Array (Array (Maybe Polygon))
plotFunc = toNumber >>> (_ / 10.0) >>> plotBenchmark defaultSize defaultExpression defaultBounds

defaultExpression :: String
defaultExpression = "x"

defaultSize :: Size
defaultSize = { width: toRational 800, height: toRational 500 }

defaultBounds :: XYBounds
defaultBounds = unitBounds

plotBenchmark :: Size -> String -> XYBounds -> Number -> Array (Array (Maybe Polygon))
plotBenchmark canvasSize exprString bounds accuracyTarget = do
  case parse exprString of
    Left _ -> []
    Right expression -> plotEnclosures { canvasSize, bounds, domainSegments, accuracyTarget, evaluator: evaluateWithXApprox, evaluator2: evaluateWithX2 }
      where
      evaluateWithXApprox :: Approx -> Maybe (ValueAndDerivative Approx)
      evaluateWithXApprox x = value
        where
        variableMap = [ Tuple "x" { value: x, derivative: one } ]

        value = case evaluateDerivative variableMap expression of
          Left _ -> Nothing
          Right v -> Just v

      evaluateWithX2 :: Approx -> Maybe (ValueAndDerivative2 Approx)
      evaluateWithX2 x = value
        where
        variableMap = [ Tuple "x" { value: x, derivative: one, derivative2: zero } ]

        value = case evaluateDerivative2 variableMap expression of
          Left _ -> Nothing
          Right v -> Just v

      domainSegments = segmentDomain { accuracyTarget, evaluator: evaluateWithX expression, l: bounds.xBounds.lower, u: bounds.xBounds.upper }

module Benchmark.Plotter where

import Prelude
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Components.BoundsInput (unitBounds)
import Data.Array ((..))
import Data.Array.NonEmpty (singleton, toNonEmpty)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Expression.Error (expectToMaybe)
import Expression.Evaluate.AutomaticDifferentiator (ValueAndDerivative, ValueAndDerivative2, evaluateDerivative, evaluateDerivative2)
import Expression.Parser (parse)
import IntervalArith.Approx (Approx)
import IntervalArith.Misc (toRational)
import Plot.FunctionSegments (segmentFunctionDomain)
import Plot.RobustFunctionPlot (plotEnclosures)
import Plot.RoughFunctionPlot (evaluateWithX)
import Test.QuickCheck.Gen (elements)
import Types (Polygon, Size, XYBounds)

benchPlot :: Benchmark
benchPlot =
  mkBenchmark
    { slug: "plot"
    , title: "Plotting the function 'f(x)=" <> defaultExpression <> "'"
    , sizes: (5 .. 25)
    , sizeInterpretation: "Accuracy target x10"
    , inputsPerSize: 10
    , gen: elements <<< toNonEmpty <<< singleton
    , functions:
        [ benchFn "plotEnclosures and segmentDomain" plotFunc
        ]
    }

-- | Divide the given `Int` by 100 and use it as the accuracy of the plot
plotFunc :: Int -> Array (Array (Maybe Polygon))
plotFunc = toNumber >>> (_ / 10.0) >>> plotBenchmark defaultSize defaultExpression defaultBounds

defaultExpression :: String
defaultExpression = "sin(10*x)"

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
      evaluateWithXApprox x = expectToMaybe $ evaluateDerivative variableMap expression
        where
        variableMap = [ Tuple "x" { value: x, derivative: one } ]

      evaluateWithX2 :: Approx -> Maybe (ValueAndDerivative2 Approx)
      evaluateWithX2 x = expectToMaybe $ evaluateDerivative2 variableMap expression
        where
        variableMap = [ Tuple "x" { value: x, derivative: one, derivative2: zero } ]

      domainSegments = segmentFunctionDomain { accuracyTarget, evaluator: evaluateWithX expression, l: bounds.xBounds.lower, u: bounds.xBounds.upper }

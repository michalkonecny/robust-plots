module Benchmark.Plotter where

import Prelude
import Benchotron.Core (Benchmark, benchFn, mkBenchmark)
import Components.BoundsInput (unitBounds)
import Data.Array (concat, (..))
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Expression.Parser (parse)
import IntervalArith.Misc (toRational)
import Plot.PlotEvaluator (approxExpressionEvaluator, numberExpressionEvaluator)
import Plot.RobustPlot (plotEnclosures)
import Plot.Segments (segmentDomain)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)
import Types (Polygon, Size, XYBounds)

benchPlot :: Benchmark
benchPlot =
  mkBenchmark
    { slug: "plot"
    , title: "Plotting the sample expression"
    , sizes: (1 .. 5)
    , sizeInterpretation: "Upper bound of accuracy range with 0.1 interval"
    , inputsPerSize: 1
    , gen: \n -> vectorOf n arbitrary
    , functions:
        [ benchFn "plotEnclosures" plotEclosuresBench
        ]
    }

plotEclosuresBench :: Array Int -> Array (Maybe Polygon)
plotEclosuresBench = concat <<< map plotFunc

-- | Divide the given `Int` by 10 and use it as the accuracy of the plot
plotFunc :: Int -> Array (Maybe Polygon)
plotFunc = toNumber >>> (_ / 10.0) >>> plotBenchmark defaultSize defaultExpression defaultBounds

defaultExpression :: String
defaultExpression = "x"

defaultSize :: Size
defaultSize = { width: toRational 800, height: toRational 500 }

defaultBounds :: XYBounds
defaultBounds = unitBounds

plotBenchmark :: Size -> String -> XYBounds -> Number -> Array (Maybe Polygon)
plotBenchmark size exprString bounds accuracy = do
  case parse exprString of
    Left _ -> []
    Right expression -> do
      let
        segments = segmentDomain accuracy (numberExpressionEvaluator expression) bounds.xBounds.lower bounds.xBounds.upper
      plotEnclosures size bounds segments (approxExpressionEvaluator expression)

module Test.Plot.RobustPlot.SegmentDomain
  ( segmentDomainTests
  ) where

import Prelude
import Data.Array (length)
import Data.Either (Either(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Expression.Parser (parse)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsNumber)
import Plot.JobBatcher (initialJobQueue)
import Plot.PlotEvaluator (numberExpressionEvaluator)
import Plot.RobustPlot (segmentDomain)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

segmentDomainTests :: TestSuite
segmentDomainTests =
  suite "Plot.RobustPlot - segmentDomain" do
    test "SHOULD segment domain into 32 segments WHEN f''(x) = 0 AND accuracyTarget = 1 AND onePixel = 1 AND domain = [-1, 1]" do
      let
        -- given
        jobQueue = initialJobQueue

        expression = parseAndSimplify "x"

        evaluator = numberExpressionEvaluator expression

        accuracyTarget = 0.1

        l = -one

        u = one

        segments = segmentDomain accuracyTarget evaluator l u

        -- then
        expected =
          "(-1.0,-0.9375),"
            <> "(-0.9375,-0.875),"
            <> "(-0.875,-0.8125),"
            <> "(-0.8125,-0.75),"
            <> "(-0.75,-0.6875),"
            <> "(-0.6875,-0.625),"
            <> "(-0.625,-0.5625),"
            <> "(-0.5625,-0.5),"
            <> "(-0.5,-0.4375),"
            <> "(-0.4375,-0.375),"
            <> "(-0.375,-0.3125),"
            <> "(-0.3125,-0.25),"
            <> "(-0.25,-0.1875),"
            <> "(-0.1875,-0.125),"
            <> "(-0.125,-0.0625),"
            <> "(-0.0625,0.0),"
            <> "(0.0,0.0625),"
            <> "(0.0625,0.125),"
            <> "(0.125,0.1875),"
            <> "(0.1875,0.25),"
            <> "(0.25,0.3125),"
            <> "(0.3125,0.375),"
            <> "(0.375,0.4375),"
            <> "(0.4375,0.5),"
            <> "(0.5,0.5625),"
            <> "(0.5625,0.625),"
            <> "(0.625,0.6875),"
            <> "(0.6875,0.75),"
            <> "(0.75,0.8125),"
            <> "(0.8125,0.875),"
            <> "(0.875,0.9375),"
            <> "(0.9375,1.0)"

        expectedCount = 32
      equal expectedCount $ length segments
      equal expected $ showSegments segments

showSegments :: Array Approx -> String
showSegments = (joinWith ",") <<< (map showSegment)
  where
  showSegment :: Approx -> String
  showSegment a = "(" <> (show l) <> "," <> (show u) <> ")"
    where
    (Tuple l u) = boundsNumber a

parseAndSimplify :: String -> Expression
parseAndSimplify rawExpression = expression
  where
  expressionOrParseError = parse rawExpression

  expression = case expressionOrParseError of
    Right e -> simplify e
    Left error -> unsafeThrow $ show error

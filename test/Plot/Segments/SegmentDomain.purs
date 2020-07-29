module Test.Plot.FunctionSegments.SegmentDomain
  ( segmentDomainTests
  ) where

import Prelude
import Data.Array (length)
import Data.Either (Either(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..), snd)
import Effect.Exception.Unsafe (unsafeThrow)
import Expression.Parser (parse)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import IntervalArith.Approx (Approx, boundsNumber)
import Plot.Commands (Depth)
import Plot.JobBatcher (initialJobQueue)
import Plot.RoughFunctionPlot (evaluateWithX)
import Plot.FunctionSegments (segmentDomain)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)

segmentDomainTests :: TestSuite
segmentDomainTests =
  suite "Plot.FunctionSegments - segmentDomain" do
    test "SHOULD segment domain into 8 segments WHEN f''(x) = 0 AND accuracyTarget = 0.1 AND onePixel = 1 AND domain = [-1, 1]" do
      let
        -- given
        jobQueue = initialJobQueue

        expression = parseAndSimplify "x"

        evaluator = evaluateWithX expression

        accuracyTarget = 0.1

        l = -one

        u = one

        segments = segmentDomain { accuracyTarget, evaluator, l, u }

        -- then
        expected = "(-1.0,-0.75),(-0.75,-0.5),(-0.5,-0.25),(-0.25,0.0),(0.0,0.25),(0.25,0.5),(0.5,0.75),(0.75,1.0)"

        expectedCount = 8
      equal expectedCount $ length segments
      equal expected $ showSegments segments

showSegments :: Array (Tuple Depth Approx) -> String
showSegments = (joinWith ",") <<< (map (showSegment <<< snd))
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

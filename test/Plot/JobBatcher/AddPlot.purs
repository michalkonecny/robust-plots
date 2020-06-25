module Test.Plot.JobBatcher.AddPlot
  ( addPlotTests
  ) where

import Prelude
import Components.Main.Helper (initialBounds)
import Data.Either (Either(..))
import Data.List (length, (!!))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Expression.Error (Expect, throw)
import Expression.Parser (parse)
import Expression.Simplifier (simplify)
import Expression.Syntax (Expression)
import Plot.Commands (PlotCommand(..), robustPlot)
import Plot.JobBatcher (Job, addPlot, initialJobQueue)
import Plot.Queue (toList)
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert (equal)
import Types (Id, XYBounds)
import IntervalArith.Misc (Rational, big)
import Data.Ratio ((%))

addPlotTests :: TestSuite
addPlotTests =
  suite "Plot.JobBatcher - addPlot" do
    test "SHOULD split into batches WHEN given Robust plot command AND batchSegmentCount = 5" do
      let
        emptyQueue = initialJobQueue

        expressionOrError = parseAndSimplify "x"

        label = "x"

        batchId = 1

        bounds = initialBounds
      case expressionOrError of
        Left error -> failure (show error)
        Right expression -> do
          let
            plotCommand = robustPlot 10 bounds expression label

            queueWithPlot = addPlot 5 emptyQueue plotCommand batchId

            checkJobWithCommonInfo = checkJob bounds label expression batchId

            jobs = toList queueWithPlot.queue
          equal 5 $ length jobs
          checkJobWithCommonInfo 1 (ratio (-1) 1) (ratio (-3) 5) (jobs !! 0)
          checkJobWithCommonInfo 2 (ratio (-3) 5) (ratio (-1) 5) (jobs !! 1)
          checkJobWithCommonInfo 3 (ratio (-1) 5) (ratio 1 5) (jobs !! 2)
          checkJobWithCommonInfo 4 (ratio 1 5) (ratio 3 5) (jobs !! 3)
          checkJobWithCommonInfo 5 (ratio 3 5) (ratio 1 1) (jobs !! 4)

checkJob :: XYBounds -> String -> Expression -> Id -> Id -> Rational -> Rational -> Maybe Job -> Aff Unit
checkJob _ _ _ _ id _ _ Nothing = failure $ "Expected Job " <> (show id) <> " but got Nothing"

checkJob fullBounds label expression batchId id lower upper (Just job) = do
  equal id job.id
  equal batchId job.batchId
  equal (RobustPlot 10 (fullBounds { xBounds = { lower, upper } }) fullBounds.xBounds expression label) job.command

parseAndSimplify :: String -> Expect Expression
parseAndSimplify rawExpression = valueOrEvaluationError
  where
  expressionOrParseError = parse rawExpression

  valueOrEvaluationError = case expressionOrParseError of
    Right expression -> pure $ simplify expression
    Left error -> throw error

ratio :: Int -> Int -> Rational
ratio n d = (big n) % (big d)

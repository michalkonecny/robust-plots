module Test.IntervalArith.Approx.Sqrt
  ( approxTests_SqrtA
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import IntervalArith.Approx (Approx(..), consistent)
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..))
import Test.TestUtils (assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

approxTests_SqrtA :: TestSuite
approxTests_SqrtA =
  suite "IntervalArith.Approx - square root" do
    test "SHOULD HOLD sqrtA 0 = 0" do
      let
        -- given
        input1 = zero

        -- when
        result = sqrtA input1

        -- then
        expected = Just zero
      equal result expected
    test "SHOULD HOLD sqrtA (-1) = Nothing" do
      let
        -- given
        input1 = (-one)

        -- when
        result = sqrtA input1

        -- then
        expected = Nothing
      equal result expected
    test "SHOULD HOLD sqrtA [0±1] = [0.5±0.5]" do
      let
        -- given
        input1 = Approx 10 (big 0) (big 1) 0

        -- when
        result = sqrtA input1

        -- then
        expected = Just $ Approx 11 (big 1025) (big 1025) (-11)
      equal result expected
    test "SHOULD HOLD (sqrt a)^2 ~ a WHEN a>=0 FOR ALL approx a"
      $ quickCheck \aPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            -- when
            result = case sqrtA a of
              Just sa -> sa * sa
              _ -> Bottom

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show a, showA a, show (sqrtA a) ]
          in
            a `consistentOp` result

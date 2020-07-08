module Test.IntervalArith.Approx.Pi
  ( approxTests_piA
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import IntervalArith.Approx (Approx(..), consistent)
import IntervalArith.Approx.Pi (piA)
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox(..))
import Test.IntervalArith.Misc (ArbitraryInt0To1000(..), ArbitraryInteger(..), ArbitraryPositiveExponent(..))
import Test.TestUtils (assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

approxTests_piA :: TestSuite
approxTests_piA =
  suite "IntervalArith.Approx - pi" do
    test "SHOULD HOLD piA 200 = 3.14159265358979323846264338327950288419716939937510582097494~" do
      let
        -- given
        input1 = piA 200

        -- when
        result = showA input1

        -- then
        expected = "3.14159265358979323846264338327950288419716939937510582097494~"
      equal result expected
    test "SHOULD HOLD piA p1 ~ piA p2 FOR ALL precisions p1, p2"
      $ quickCheck \p1Pre p2Pre ->
          let
            -- given
            (ArbitraryInt0To1000 p1) = p1Pre
            (ArbitraryInt0To1000 p2) = p2Pre

            -- when
            pi1 = piA p1
            pi2 = piA p2

            -- then
            consistentOp = assertOpWithInput consistent " `consistent` " [ show p1, show p2, show pi1, showA pi1, show pi2, showA pi2 ]
          in
            pi1 `consistentOp` pi2

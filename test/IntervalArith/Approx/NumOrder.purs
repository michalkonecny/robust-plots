module Test.IntervalArith.Approx.NumOrder
  ( approxTests_NumOrder
  ) where

import Prelude
import Data.Ratio ((%))
import IntervalArith.Approx (Approx(..), fromInteger, fromRationalPrec, lowerA)
import IntervalArith.Approx.NumOrder (absA, (!<=!), (!>=!), (?<=?), (?>=?))
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox, approxEqParams)
import Test.Order (preOrderTests)
import Test.QuickCheck ((<?>))
import Test.QuickCheck.Combinators ((&=&))
import Test.TestUtils (SuiteOrdParams1, assertOpWithInput, extendRecord)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

approxTests_NumOrder :: TestSuite
approxTests_NumOrder =
  suite "IntervalArith.Approx - partial numeric order" do
    test "SHOULD HOLD fromRational (1/3) !≰! fromRational (1/3)" do
      let
        -- given
        input = fromRationalPrec 10 ((big 1) % (big 3))

        -- when
        result = input !<=! input

        -- then
        expected = false
      equal expected result
    test "SHOULD HOLD fromInteger n <= fromInteger m FOR ALL integers n <= m"
      $ quickCheck \n m ->
          let
            -- given
            nA = fromInteger (big $ min n m)

            mA = fromInteger (big $ max n m)
          in
            ((nA !<=! mA) <?> "nA !<=! mA")
              &=& ((nA ?<=? mA) <?> "nA ?<=? mA")
              &=& ((mA !>=! nA) <?> "mA !>=! nA")
              &=& ((mA ?>=? nA) <?> "mA ?>=? nA")
    preOrderTests approxNumOrdMaybeParams

approxNumOrdMaybeParams :: SuiteOrdParams1 ArbitraryApprox Approx
approxNumOrdMaybeParams =
  approxNumOrdSureParams
    { leqOpWithInput = (assertOpWithInput (?<=?) " ?≤? ")
    , leqOpSymbol = "?≤?"
    }

approxNumOrdSureParams :: SuiteOrdParams1 ArbitraryApprox Approx
approxNumOrdSureParams =
  approxEqParams
    `extendRecord`
      { suitePrefix: "IntervalArith.Approx <="
      , leqOpWithInput: (assertOpWithInput (!<=!) " !≤! ")
      , leqOpSymbol: "!≤!"
      , makeLeq:
          \a b -> case b of
            Bottom -> Bottom
            _ -> (lowerA b) - (absA a)
      }

module Test.IntervalArith.Approx.NumOrder
  ( approxTests_NumOrder
  ) where

import Prelude
import Data.Ratio ((%))
import IntervalArith.Approx (Approx(..), fromInteger, fromRationalPrec, lowerA)
import IntervalArith.Approx.NumOrder (absA, (!<=!), (!>=!), (?<=?), (?>=?))
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Misc (big)
import Test.IntervalArith.Approx.Arbitrary (ArbitraryApprox, approxEqParams)
import Test.IntervalArith.Misc (ArbitraryPositiveExponent(..), ArbitraryRational(..))
import Test.Order (reflexivity, transitivity)
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
    test "SHOULD HOLD fromRational (1/3) ?<=? fromRational (1/3)" do
      let
        -- given
        input = fromRationalPrec 10 ((big 1) % (big 3))

        -- when
        result = input ?<=? input

        -- then
        expected = true
      equal expected result
    test "SHOULD HOLD fromInteger n !<=! fromInteger m FOR ALL integers n <= m"
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
    test "SHOULD HOLD fromRational a ?<=? fromRational b FOR ALL rationals a <= b"
      $ quickCheck \aPre bPre precPre ->
          let
            -- given
            (ArbitraryRational a) = aPre

            (ArbitraryRational b) = bPre

            (ArbitraryPositiveExponent prec) = precPre

            aA = fromRationalPrec prec (min a b)

            bA = fromRationalPrec prec (max a b)

            inputDescription = "; aA = " <> showA aA <> "; bA = " <> showA bA
          in
            ((aA ?<=? bA) <?> "aA ?<=? bA" <> inputDescription)
              &=& ((bA ?>=? aA) <?> "bA ?>=? aA" <> inputDescription)
    transitivity approxNumOrdSureParams
    reflexivity approxNumOrdMaybeParams

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

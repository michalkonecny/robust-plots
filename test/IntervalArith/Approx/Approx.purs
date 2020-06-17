module Test.IntervalArith.Approx
  ( approxTests, ArbitraryApprox(..), randomSampleApprox
  ) where

import Prelude
import Data.NonEmpty (foldl1, (:|))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import IntervalArith.Approx (Approx(..), approxMB, boundsR, consistent, fromRationalBoundsPrec, fromRationalPrec, setMB, (⊑))
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Extended (Extended(..))
import IntervalArith.Misc (big)
import Test.Field (fieldTests)
import Test.IntervalArith.Approx.ShowA (approxTests_showA)
import Test.IntervalArith.Misc (ArbitraryInteger(..), ArbitraryPositiveExponent(..), ArbitraryRational(..))
import Test.Order (preOrderTests)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Combinators ((&=&))
import Test.QuickCheck.Gen (Size, chooseInt, randomSample', sized)
import Test.TestUtils (SuiteOrdParams1, SuiteEqParams1, assertOp, assertOpWithInput)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.QuickCheck (quickCheck)

data ArbitraryApprox
  = ArbitraryApprox Approx

instance arbitraryApprox :: Arbitrary ArbitraryApprox where
  arbitrary =
    sized \size -> do
      (ArbitraryInteger m) <- arbitrary
      s <- chooseInt (-2 * size) (2 * size)
      mb <- chooseInt 10 (10 + 2 * size)
      pure $ ArbitraryApprox $ approxMB mb m (big 0) s

randomSampleApprox :: Size -> Effect (Array String)
randomSampleApprox size = randomSample' size (map (\(ArbitraryApprox a) -> a) arbitrary) >>= \as -> pure (map showA as)

approxTests :: TestSuite
approxTests = do
  approxTests_showA
  approxTests_setMBworse
  approxTests_Order
  approxTests_Consistent
  approxTests_Field
  approxTests_fromRational
  approxTests_fromRationalBounds

approxTests_setMBworse :: TestSuite
approxTests_setMBworse =
  suite "IntervalArith.Approx - setMB" do
    test "SHOULD HOLD setMB mb a ⊑ a FOR ALL approx a and integer mb>=0"
      $ quickCheck \aPre mbPre ->
          let
            -- given
            (ArbitraryApprox a) = aPre

            (ArbitraryPositiveExponent mb) = mbPre

            -- when
            aMB = setMB mb a
          -- then
          in
            assertOp (⊑) " ⊑ " aMB a

approxTests_Order :: TestSuite
approxTests_Order =
  suite "IntervalArith.Approx - approximation order (`better`)" do
    test "SHOULD give (Approx 4 12 1 3 = 96±8 \"1~~\") WHEN setMB 4 (Approx 10 100 0 0 ~ \"100\")" do
      let
        -- given
        input = Approx 10 (big 100) (big 0) 0

        -- when
        result = Approx 4 (big 12) (big 1) 3

        -- then
        expected = result
      equal expected result
    preOrderTests approxOrdParams

approxTests_Consistent :: TestSuite
approxTests_Consistent =
  suite "IntervalArith.Approx - consistency check (`consistent`)" do
    test "SHOULD HOLD consistent (Approx 4 12 1 3 = 96±8) (Approx 4 13 1 3 = 104±8)" do
      let
        -- given
        input1 = Approx 4 (big 12) (big 1) 3

        input2 = Approx 4 (big 12) (big 1) 3

        -- when
        result = consistent input1 input2
      -- then
      equal result true
    test "SHOULD HOLD consistent a b WHEN a ⊑ b FOR ALL approx a b"
      $ quickCheck \aPre bPre ->
          let
            -- given
            (ArbitraryApprox b) = bPre

            (ArbitraryApprox a1) = aPre

            -- when
            a = approxOrdParams.makeLeq b a1
          -- then
          in
            assertOp consistent " `consistent` " a b

approxTests_fromRational :: TestSuite
approxTests_fromRational =
  suite "IntervalArith.Approx - conversion from Rational" do
    test "SHOULD HOLD addition on Approx is consistent with addition on Rational"
      $ quickCheck \aPre bPre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b) = bPre

            (ArbitraryRational a) = aPre

            -- when
            resultAddConvert = fromRationalPrec prec (a + b)

            resultConvertAdd = (fromRationalPrec prec a) + (fromRationalPrec prec b)

            -- then
            consistentOp = assertOp consistent " `consistent` "
          in
            resultAddConvert `consistentOp` resultConvertAdd
    test "SHOULD HOLD multiplication on Approx is consistent with multiplication on Rational"
      $ quickCheck \aPre bPre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b) = bPre

            (ArbitraryRational a) = aPre

            -- when
            resultMulConvert = fromRationalPrec prec (a * b)

            resultConvertMul = (fromRationalPrec prec a) * (fromRationalPrec prec b)

            -- then
            consistentOp = assertOp consistent " `consistent` "
          in
            resultMulConvert `consistentOp` resultConvertMul
    test "SHOULD HOLD division on Approx is consistent with division on Rational"
      $ quickCheck \aPre bPre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b) = bPre

            (ArbitraryRational a) = aPre

            -- when
            resultDivConvert =
              if b == zero then
                Bottom
              else
                fromRationalPrec prec (a / b)

            resultConvertDiv = (fromRationalPrec prec a) / (fromRationalPrec prec b)

            -- then
            consistentOp = assertOp consistent " `consistent` "
          in
            resultDivConvert `consistentOp` resultConvertDiv

approxTests_fromRationalBounds :: TestSuite
approxTests_fromRationalBounds =
  suite "IntervalArith.Approx - conversion from Rational bounds" do
    test "SHOULD HOLD Approx from bounds is consistent with each bound"
      $ quickCheck \a1Pre a2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational a1) = a1Pre

            (ArbitraryRational a2) = a2Pre

            aL = min a1 a2

            aU = max a1 a2

            -- when
            hull = fromRationalBoundsPrec prec aL aU

            Tuple hullL hullU = boundsR hull

            -- then
            leqOp = assertOp (<=) " <= "
          in
            hullL `leqOp` (Finite aL) &=& (Finite aU) `leqOp` hullU
    test "SHOULD HOLD addition on Approx is consistent with addition on Rational intervals"
      $ quickCheck \a1Pre a2Pre b1Pre b2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b1) = b1Pre

            (ArbitraryRational b2) = b2Pre

            bL = min b1 b2

            bU = max b1 b2

            (ArbitraryRational a1) = a1Pre

            (ArbitraryRational a2) = a2Pre

            aL = min a1 a2

            aU = max a1 a2

            -- when
            a = fromRationalBoundsPrec prec aL aU

            b = fromRationalBoundsPrec prec bL bU

            Tuple resultL resultU = boundsR $ a + b

            -- then
            leqOp = assertOpWithInput (<=) " <= " [ aL, aU, bL, bU ]
          in
            resultL `leqOp` (Finite $ aL + bL) &=& (Finite $ aU + bU) `leqOp` resultU
    test "SHOULD HOLD multiplication on Approx is multiplication on Rational intervals"
      $ quickCheck \a1Pre a2Pre b1Pre b2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b1) = b1Pre

            (ArbitraryRational b2) = b2Pre

            bL = min b1 b2

            bU = max b1 b2

            (ArbitraryRational a1) = a1Pre

            (ArbitraryRational a2) = a2Pre

            aL = min a1 a2

            aU = max a1 a2

            -- when
            a = fromRationalBoundsPrec prec aL aU

            b = fromRationalBoundsPrec prec bL bU

            Tuple resultL resultU = boundsR $ a * b

            products = (aL * bL) :| [ aL * bU, aU * bL, aU * bU ]

            abL = foldl1 min products

            abU = foldl1 max products

            -- then
            leqOp = assertOpWithInput (<=) " <= " [ aL, aU, bL, bU ]
          in
            resultL `leqOp` (Finite $ abL) &=& (Finite $ abU) `leqOp` resultU
    test "SHOULD HOLD division on Approx is division on Rational intervals"
      $ quickCheck \a1Pre a2Pre b1Pre b2Pre precPre ->
          let
            -- given
            (ArbitraryPositiveExponent prec) = precPre

            (ArbitraryRational b1) = b1Pre

            (ArbitraryRational b2) = b2Pre

            bL = min b1 b2

            bU = max b1 b2

            (ArbitraryRational a1) = a1Pre

            (ArbitraryRational a2) = a2Pre

            aL = min a1 a2

            aU = max a1 a2

            a = fromRationalBoundsPrec prec aL aU

            b = fromRationalBoundsPrec prec bL bU

            -- when
            Tuple resultL resultU = boundsR $ a / b

            bLinv = if bL == zero then PosInf else Finite (one / bL)

            bUinv = if bU == zero then NegInf else Finite (one / bU)

            aLf = Finite aL

            aUf = Finite aU

            products = (aLf * bLinv) :| [ aLf * bUinv, aUf * bLinv, aUf * bUinv ]

            hasZero = bL <= zero && zero <= bU

            aDbL = if hasZero then NegInf else foldl1 min products

            aDbU = if hasZero then PosInf else foldl1 max products

            -- then
            leqOp = assertOpWithInput (<=) " <= " [ aL, aU, bL, bU ]
          in
            resultL `leqOp` aDbL &=& aDbU `leqOp` resultU

approxTests_Field :: TestSuite
approxTests_Field = fieldTests approxEqParams

approxOrdParams :: SuiteOrdParams1 ArbitraryApprox Approx
approxOrdParams =
  { suitePrefix: "IntervalArith.Approx ⊑"
  , valuesName: "interval approximations"
  , fromArbitraryValue: \(ArbitraryApprox d) -> d
  , leqOpWithInput: (assertOpWithInput (⊑) " ⊑ ")
  , leqOpSymbol: "⊑"
  , eqOpWithInput: (assertOpWithInput (==) " == ")
  , eqOpSymbol: "="
  , makeLeq:
      \a b -> case b of
        (Approx mb _ _ _) -> setMB mb a
        Bottom -> Bottom
  }

approxEqParams :: SuiteEqParams1 ArbitraryApprox Approx
approxEqParams =
  { suitePrefix: "IntervalArith.Approx - "
  , valuesName: "interval approximations"
  , fromArbitraryValue: \(ArbitraryApprox d) -> d
  , eqOpWithInput: (assertOpWithInput consistent " ~ ")
  , eqOpSymbol: "~"
  }

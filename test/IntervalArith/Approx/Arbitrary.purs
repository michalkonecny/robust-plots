module Test.IntervalArith.Approx.Arbitrary
  ( ArbitraryApprox(..)
  , randomSampleApprox
  , approxOrdParams
  , approxEqParams
  ) where

import Prelude
import Effect (Effect)
import IntervalArith.Approx (Approx(..), approxMB, consistent, setMB, (⊑))
import IntervalArith.Approx.ShowA (showA)
import IntervalArith.Misc (big)
import Test.IntervalArith.Misc (ArbitraryInteger(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Size, chooseInt, randomSample', sized)
import Test.TestUtils (SuiteOrdParams1, SuiteEqParams1, assertOpWithInput)

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

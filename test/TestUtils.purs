module Test.TestUtils where

import Prelude

import Test.QuickCheck (Result, (<?>))

type SuiteEqParams1 at t
  = { suitePrefix :: String
    , valuesName :: String
    , fromArbitraryValue :: (at -> t)
    , eqOp :: (t -> t -> Result)
    , eqOpSymbol :: String
    }

type SuiteOrdParams1 at t
  = { suitePrefix :: String
    , valuesName :: String
    , fromArbitraryValue :: (at -> t)
    , leqOp :: (t -> t -> Result)
    , leqOpSymbol :: String
    , eqOp :: (t -> t -> Result)
    , eqOpSymbol :: String
    }

-- copied from source code of purescript-quickcheck Test.QuickCheck.
assertOp :: forall a. Eq a => Show a => (a -> a -> Boolean) -> String -> a -> a -> Result
assertOp op failString a b = a `op` b <?> show a <> failString <> show b

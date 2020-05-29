module Test.TestUtils where

import Prelude

import Data.String (joinWith)
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
assertOp op failString a b =
  a `op` b
    <?> show a
    <> failString
    <> show b

-- copied from source code of purescript-quickcheck Test.QuickCheck.
assertOpMoreHelp :: forall a. Eq a => Show a => (a -> a -> Boolean) -> String -> String -> a -> a -> Result
assertOpMoreHelp op opString moreHelp a b =
  a `op` b
    <?> show a
    <> opString
    <> show b
    <> moreHelp

assertOpWithInput :: 
  forall t1 t2. Eq t1 => Show t1 => Show t2 => 
  (t1 -> t1 -> Boolean) -> String -> Array t2 -> t1 -> t1 -> Result
assertOpWithInput op opString input = assertOpMoreHelp op opString moreHelp
  where
  moreHelp = "; " <> (joinWith ", " $ map show input)

eqWithInput :: forall t1 t2. Eq t2 => Show t2 => Show t1 => Array t1 -> t2 -> t2 -> Result
eqWithInput = assertOpWithInput (==) " == "

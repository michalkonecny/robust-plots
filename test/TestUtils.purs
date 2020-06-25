module Test.TestUtils where

import Prelude
import Data.String (joinWith)
import Prim.Row (class Nub, class Union)
import Record.Builder (build, merge)
import Test.QuickCheck (Result, (<?>))

type SuiteEqParams1 at t
  = { suitePrefix :: String
    , valuesName :: String
    , fromArbitraryValue :: (at -> t)
    , eqOpWithInput :: Array String -> (t -> t -> Result)
    , eqOpSymbol :: String
    }

type SuiteOrdParams1 at t
  = { suitePrefix :: String
    , valuesName :: String
    , fromArbitraryValue :: (at -> t)
    , leqOpWithInput :: Array String -> (t -> t -> Result)
    , leqOpSymbol :: String
    , eqOpWithInput :: Array String -> (t -> t -> Result)
    , eqOpSymbol :: String
    , makeLeq :: t -> t -> t -- ^ `(makeLeq a b) <= a` and `(makeLeq a b)` should be similar to `b`
    }

extendRecord :: forall t3 t4 t6 t7. Union t4 t7 t6 => Nub t6 t3 => Record t7 -> Record t4 -> Record t3
extendRecord rec1 rec2 = build (merge rec1) rec2

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
  forall t1.
  Eq t1 =>
  Show t1 =>
  (t1 -> t1 -> Boolean) -> String -> Array String -> t1 -> t1 -> Result
assertOpWithInput op opString input = assertOpMoreHelp op opString moreHelp
  where
  moreHelp = "; " <> (joinWith ", " $ map show input)

eqWithInput :: forall t2. Eq t2 => Show t2 => Array String -> t2 -> t2 -> Result
eqWithInput = assertOpWithInput (==) " == "

leqWithInput :: forall t2. Ord t2 => Show t2 => Array String -> t2 -> t2 -> Result
leqWithInput = assertOpWithInput (<=) " <= "

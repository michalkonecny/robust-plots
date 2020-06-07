module Test.Order where

import Prelude
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Combinators ((&=&), (==>), (|=|))
import Test.TestUtils (SuiteOrdParams1)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

totalOrderTests ::
  forall at t. Arbitrary at => SuiteOrdParams1 at t -> TestSuite
totalOrderTests params = do
  suite (params.suitePrefix <> " is a total order") do
    connexity params
    transitivity params
    antisymmetry params

partialOrderTests ::
  forall at t. Arbitrary at => SuiteOrdParams1 at t -> TestSuite
partialOrderTests params =
  suite (params.suitePrefix <> " is a partial order") do
    reflexivity params
    transitivity params
    antisymmetry params

preOrderTests ::
  forall at t. Arbitrary at => SuiteOrdParams1 at t -> TestSuite
preOrderTests params =
  suite (params.suitePrefix <> " is a pre-order") do
    reflexivity params
    transitivity params

reflexivity ::
  forall at t. Arbitrary at => SuiteOrdParams1 at t -> TestSuite
reflexivity { suitePrefix, valuesName, fromArbitraryValue, leqOpWithInput, leqOpSymbol, eqOpWithInput, eqOpSymbol } =
  test
    ( "SHOULD HOLD reflexivity: "
        <> "a"
        <> leqOpSymbol
        <> "a"
        <> " FOR ALL "
        <> valuesName
        <> " a"
    )
    $ quickCheck \aA ->
        let
          a = fromArbitraryValue aA

          leqOp = leqOpWithInput [ a ]
        in
          (a) `leqOp` (a)

connexity ::
  forall at t. Arbitrary at => SuiteOrdParams1 at t -> TestSuite
connexity { suitePrefix, valuesName, fromArbitraryValue, leqOpWithInput, leqOpSymbol, eqOpWithInput, eqOpSymbol } =
  test
    ( "SHOULD HOLD connexity: "
        <> "a"
        <> leqOpSymbol
        <> "b"
        <> " or "
        <> "b"
        <> leqOpSymbol
        <> "a"
        <> " FOR ALL "
        <> valuesName
        <> " a,b"
    )
    $ quickCheck \aA bA ->
        let
          a = fromArbitraryValue aA

          b = fromArbitraryValue bA

          leqOp = leqOpWithInput [ a, b ]
        in
          (a `leqOp` b) |=| (b `leqOp` a)

antisymmetry ::
  forall at t. Arbitrary at => SuiteOrdParams1 at t -> TestSuite
antisymmetry { suitePrefix, valuesName, fromArbitraryValue, leqOpWithInput, leqOpSymbol, eqOpWithInput, eqOpSymbol } =
  test
    ( "SHOULD HOLD antisymmetry: "
        <> "a"
        <> leqOpSymbol
        <> "b"
        <> " and "
        <> "b"
        <> leqOpSymbol
        <> "a"
        <> " implies "
        <> "a"
        <> eqOpSymbol
        <> "b"
        <> " FOR ALL "
        <> valuesName
        <> " a,b"
    )
    $ quickCheck \aA bA ->
        let
          a = fromArbitraryValue aA

          b = fromArbitraryValue bA

          eqOp = eqOpWithInput [ a, b ]

          leqOp = leqOpWithInput [ a, b ]
        in
          ((a `leqOp` b) &=& (b `leqOp` a)) ==> (a `eqOp` b)

transitivity ::
  forall at t. Arbitrary at => SuiteOrdParams1 at t -> TestSuite
transitivity { suitePrefix, valuesName, fromArbitraryValue, leqOpWithInput, leqOpSymbol, eqOpWithInput, eqOpSymbol } =
  test
    ( "SHOULD HOLD transitivity: "
        <> "a"
        <> leqOpSymbol
        <> "b"
        <> " and "
        <> "b"
        <> leqOpSymbol
        <> "c"
        <> " implies "
        <> "a"
        <> leqOpSymbol
        <> "c"
        <> " FOR ALL "
        <> valuesName
        <> " a,b,c"
    )
    $ quickCheck \aA bA cA ->
        let
          a = fromArbitraryValue aA

          b = fromArbitraryValue bA

          c = fromArbitraryValue cA

          leqOp = leqOpWithInput [ a, b, c ]
        in
          ((a `leqOp` b) &=& (b `leqOp` c)) ==> (a `leqOp` c)

module Test.Order where

import Prelude
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Combinators ((&=&), (==>), (|=|))
import Test.TestUtils (SuiteOrdParams1)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

totalOrderTests ::
  forall at t. Arbitrary at => Show t => SuiteOrdParams1 at t -> TestSuite
totalOrderTests params = do
  suite (params.suitePrefix <> " is a total order") do
    connexity params
    transitivity params
    antisymmetry params

partialOrderTests ::
  forall at t. Arbitrary at => Show t => SuiteOrdParams1 at t -> TestSuite
partialOrderTests params =
  suite (params.suitePrefix <> " is a partial order") do
    reflexivity params
    transitivity params
    antisymmetry params

preOrderTests ::
  forall at t. Arbitrary at => Show t => SuiteOrdParams1 at t -> TestSuite
preOrderTests params =
  suite (params.suitePrefix <> " is a pre-order") do
    reflexivity params
    transitivity params

reflexivity ::
  forall at t. Arbitrary at => Show t => SuiteOrdParams1 at t -> TestSuite
reflexivity p =
  test
    ( "SHOULD HOLD reflexivity: "
        <> "a"
        <> p.leqOpSymbol 
        <> "a"
        <> " FOR ALL "
        <> p.valuesName
        <> " a"
    )
    $ quickCheck \aA ->
        let
          a = p.fromArbitraryValue aA

          leqOp = p.leqOpWithInput [ show a ]
        in
          a `leqOp` a

connexity ::
  forall at t. Arbitrary at => Show t => SuiteOrdParams1 at t -> TestSuite
connexity p =
  test
    ( "SHOULD HOLD connexity: "
        <> "a"
        <> p.leqOpSymbol
        <> "b"
        <> " or "
        <> "b"
        <> p.leqOpSymbol
        <> "a"
        <> " FOR ALL "
        <> p.valuesName
        <> " a,b"
    )
    $ quickCheck \aA bA ->
        let
          a = p.fromArbitraryValue aA

          b = p.fromArbitraryValue bA

          leqOp = p.leqOpWithInput $ map show [ a, b ]
        in
          (a `leqOp` b) |=| (b `leqOp` a)

antisymmetry ::
  forall at t. Arbitrary at => Show t => SuiteOrdParams1 at t -> TestSuite
antisymmetry p =
  test
    ( "SHOULD HOLD antisymmetry: "
        <> "a"
        <> p.leqOpSymbol
        <> "b"
        <> " and "
        <> "b"
        <> p.leqOpSymbol
        <> "a"
        <> " implies "
        <> "a"
        <> p.eqOpSymbol
        <> "b"
        <> " FOR ALL "
        <> p.valuesName
        <> " a,b"
    )
    $ quickCheck \aA bA ->
        let
          a = p.fromArbitraryValue aA

          b1 = p.fromArbitraryValue bA

          b2 = p.makeLeq a b1

          b = p.makeLeq b1 b2

          eqOp = p.eqOpWithInput $ map show [ a, b ]

          leqOp = p.leqOpWithInput $ map show [ a, b ]
        in
          ((a `leqOp` b) &=& (b `leqOp` a)) ==> (a `eqOp` b)

transitivity ::
  forall at t. Arbitrary at => Show t => SuiteOrdParams1 at t -> TestSuite
transitivity p =
  test
    ( "SHOULD HOLD transitivity: "
        <> "a"
        <> p.leqOpSymbol
        <> "b"
        <> " and "
        <> "b"
        <> p.leqOpSymbol
        <> "c"
        <> " implies "
        <> "a"
        <> p.leqOpSymbol
        <> "c"
        <> " FOR ALL "
        <> p.valuesName
        <> " a,b,c"
    )
    $ quickCheck \aA bA cA ->
        let
          c = p.fromArbitraryValue cA

          b = p.makeLeq c $ p.fromArbitraryValue bA

          a = p.makeLeq b $ p.fromArbitraryValue aA

          leqOp = p.leqOpWithInput $ map show [ a, b, c ]
        in
          ((a `leqOp` b) &=& (b `leqOp` c)) ==> (a `leqOp` c)

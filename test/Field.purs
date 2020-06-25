module Test.Field where

import Prelude

import IntervalArith.Approx (consistent)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Combinators ((&=&), (|=|))
import Test.Ring (commutativeRingTests)
import Test.TestUtils (SuiteEqParams1)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.QuickCheck (quickCheck)

fieldTests ::
  forall at t. Arbitrary at => Field t => Show t => SuiteEqParams1 at t -> TestSuite
fieldTests params =
  suite (params.suitePrefix <> " forms a field") do
    commutativeRingTests params
    divisionRingLaws params

divisionRingLaws ::
  forall at t. Arbitrary at => DivisionRing t => Show t => SuiteEqParams1 at t -> TestSuite
divisionRingLaws p =
  suite (p.suitePrefix <> " satisfies division ring laws") do
    test
      ( "SHOULD HOLD multiplicative inverse: recip a * a "
          <> p.eqOpSymbol
          <> " a * recip a "
          <> p.eqOpSymbol
          <> " one "
          <> "FOR ALL "
          <> p.valuesName
          <> " a "
          <> "!" <> p.eqOpSymbol
          <> " zero"
      )
      $ quickCheck \aA ->
          let
            a = p.fromArbitraryValue aA

            eqOp = p.eqOpWithInput [ show a ]
          in
            (a `eqOp` zero) |=| 
            (((recip a * a) `eqOp` one)
              &=& ((a * recip a) `eqOp` one))
    test
      ( "SHOULD HOLD non-zero ring: " <>
          "zero " <> "!" <> p.eqOpSymbol <> " one"
      ) 
      $ assert "Could not separate 0 from 1"
        $ not $ consistent zero one

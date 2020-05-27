module Test.Ring where

import Prelude
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Combinators ((&=&))
import Test.TestUtils (SuiteEqParams1)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

commutativeRingTests ::
  forall at t. Arbitrary at => CommutativeRing t => SuiteEqParams1 at t -> TestSuite
commutativeRingTests params =
  suite (params.suitePrefix <> " forms a commutative ring") do
    commutativeMonoidAddition params
    commutativeMonoidMultiplication params
    distributiveLaws params
    otherLaws params

-- distributiveRules params
commutativeMonoidAddition ::
  forall at t. Arbitrary at => Semiring t => SuiteEqParams1 at t -> TestSuite
commutativeMonoidAddition { suitePrefix, valuesName, fromArbitraryValue, eqOp, eqOpSymbol } =
  suite (suitePrefix <> " forms a commutative monoid under addition") do
    test
      ( "SHOULD HOLD associativity: (a + b) + c "
          <> eqOpSymbol
          <> " a + (b + c) FOR ALL "
          <> valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = fromArbitraryValue aA

            b = fromArbitraryValue bA

            c = fromArbitraryValue cA
          in
            ((a + b) + c) `eqOp` (a + (b + c))
    test
      ( "SHOULD HOLD identity: zero + a "
          <> eqOpSymbol
          <> " a + zero "
          <> eqOpSymbol
          <> " a "
          <> "FOR ALL "
          <> valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = fromArbitraryValue aA
          in
            ((a + zero) `eqOp` a)
              &=& (a `eqOp` (a + zero))
    test
      ( "SHOULD HOLD commutativity: a + b "
          <> eqOpSymbol
          <> " b + a "
          <> "FOR ALL "
          <> valuesName
          <> " a, b"
      )
      $ quickCheck \aA bA ->
          let
            a = fromArbitraryValue aA

            b = fromArbitraryValue bA
          in
            (a + b) `eqOp` (b + a)

commutativeMonoidMultiplication ::
  forall at t. Arbitrary at => CommutativeRing t => SuiteEqParams1 at t -> TestSuite
commutativeMonoidMultiplication { suitePrefix, valuesName, fromArbitraryValue, eqOp, eqOpSymbol } =
  suite (suitePrefix <> " forms a commutative monoid under multiplication") do
    test
      ( "SHOULD HOLD associativity: (a * b) * c "
          <> eqOpSymbol
          <> " a * (b * c) FOR ALL "
          <> valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = fromArbitraryValue aA

            b = fromArbitraryValue bA

            c = fromArbitraryValue cA
          in
            ((a * b) * c) `eqOp` (a * (b * c))
    test
      ( "SHOULD HOLD identity: one * a "
          <> eqOpSymbol
          <> " a * one "
          <> eqOpSymbol
          <> " a "
          <> "FOR ALL "
          <> valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = fromArbitraryValue aA
          in
            ((a * one) `eqOp` a)
              &=& (a `eqOp` (a * one))
    test
      ( "SHOULD HOLD commutativity: a * b "
          <> eqOpSymbol
          <> " b * a "
          <> "FOR ALL "
          <> valuesName
          <> " a, b"
      )
      $ quickCheck \aA bA ->
          let
            a = fromArbitraryValue aA

            b = fromArbitraryValue bA
          in
            (a * b) `eqOp` (b * a)

distributiveLaws ::
  forall at t. Arbitrary at => Semiring t => SuiteEqParams1 at t -> TestSuite
distributiveLaws { suitePrefix, valuesName, fromArbitraryValue, eqOp, eqOpSymbol } =
  suite (suitePrefix <> " satisfies semiring distributive laws") do
    test
      ( "SHOULD HOLD left distributivity: a * (b + c) "
          <> eqOpSymbol
          <> " (a * b) + (a * c) FOR ALL "
          <> valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = fromArbitraryValue aA

            b = fromArbitraryValue bA

            c = fromArbitraryValue cA
          in
            (a * (b + c)) `eqOp` ((a * b) + (a * c))
    test
      ( "SHOULD HOLD right distributivity: (a + b) * c "
          <> eqOpSymbol
          <> " (a * c) + (b * c) FOR ALL "
          <> valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = fromArbitraryValue aA

            b = fromArbitraryValue bA

            c = fromArbitraryValue cA
          in
            ((a + b) * c) `eqOp` ((a * c) + (b * c))

otherLaws ::
  forall at t. Arbitrary at => CommutativeRing t => SuiteEqParams1 at t -> TestSuite
otherLaws { suitePrefix, valuesName, fromArbitraryValue, eqOp, eqOpSymbol } =
  suite (suitePrefix <> " satisfies other ring laws") do
    test
      ( "SHOULD HOLD additive inverse: a - a "
          <> eqOpSymbol
          <> " (zero - a) + a "
          <> eqOpSymbol
          <> " zero "
          <> "FOR ALL "
          <> valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = fromArbitraryValue aA
          in
            ((a - a) `eqOp` zero)
            &=&
            (((zero - a) + a) `eqOp` zero)
    test
      ( "SHOULD HOLD annihilation: zero * a "
          <> eqOpSymbol
          <> " zero "
          <> "FOR ALL "
          <> valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = fromArbitraryValue aA
          in
            (zero * a) `eqOp` zero

module Test.Ring where

import Prelude
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Combinators ((&=&))
import Test.TestUtils (SuiteEqParams1)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

commutativeRingTests ::
  forall at t. Arbitrary at => CommutativeRing t => Show t => SuiteEqParams1 at t -> TestSuite
commutativeRingTests params =
  suite (params.suitePrefix <> " forms a commutative ring") do
    commutativeMonoidAddition params
    commutativeMonoidMultiplication params
    distributiveLaws params
    otherLaws params

-- distributiveRules params
commutativeMonoidAddition ::
  forall at t. Arbitrary at => Semiring t => Show t => SuiteEqParams1 at t -> TestSuite
commutativeMonoidAddition p =
  suite (p.suitePrefix <> " forms a commutative monoid under addition") do
    test
      ( "SHOULD HOLD associativity: (a + b) + c "
          <> p.eqOpSymbol
          <> " a + (b + c) FOR ALL "
          <> p.valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = p.fromArbitraryValue aA

            b = p.fromArbitraryValue bA

            c = p.fromArbitraryValue cA

            eqOp = p.eqOpWithInput $ map show [ a, b, c ]
          in
            ((a + b) + c) `eqOp` (a + (b + c))
    test
      ( "SHOULD HOLD identity: zero + a "
          <> p.eqOpSymbol
          <> " a + zero "
          <> p.eqOpSymbol
          <> " a "
          <> "FOR ALL "
          <> p.valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = p.fromArbitraryValue aA

            eqOp = p.eqOpWithInput [ show a ]
          in
            ((a + zero) `eqOp` a)
              &=& (a `eqOp` (a + zero))
    test
      ( "SHOULD HOLD commutativity: a + b "
          <> p.eqOpSymbol
          <> " b + a "
          <> "FOR ALL "
          <> p.valuesName
          <> " a, b"
      )
      $ quickCheck \aA bA ->
          let
            a = p.fromArbitraryValue aA

            b = p.fromArbitraryValue bA

            eqOp = p.eqOpWithInput $ map show [ a, b ]
          in
            (a + b) `eqOp` (b + a)

commutativeMonoidMultiplication ::
  forall at t. Arbitrary at => CommutativeRing t => Show t => SuiteEqParams1 at t -> TestSuite
commutativeMonoidMultiplication p =
  suite (p.suitePrefix <> " forms a commutative monoid under multiplication") do
    test
      ( "SHOULD HOLD associativity: (a * b) * c "
          <> p.eqOpSymbol
          <> " a * (b * c) FOR ALL "
          <> p.valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = p.fromArbitraryValue aA

            b = p.fromArbitraryValue bA

            c = p.fromArbitraryValue cA

            eqOp = p.eqOpWithInput $ map show [ a, b, c ]
          in
            ((a * b) * c) `eqOp` (a * (b * c))
    test
      ( "SHOULD HOLD identity: one * a "
          <> p.eqOpSymbol
          <> " a * one "
          <> p.eqOpSymbol
          <> " a "
          <> "FOR ALL "
          <> p.valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = p.fromArbitraryValue aA

            eqOp = p.eqOpWithInput [ show a ]
          in
            ((a * one) `eqOp` a)
              &=& (a `eqOp` (a * one))
    test
      ( "SHOULD HOLD commutativity: a * b "
          <> p.eqOpSymbol
          <> " b * a "
          <> "FOR ALL "
          <> p.valuesName
          <> " a, b"
      )
      $ quickCheck \aA bA ->
          let
            a = p.fromArbitraryValue aA

            b = p.fromArbitraryValue bA

            eqOp = p.eqOpWithInput $ map show [ a, b ]
          in
            (a * b) `eqOp` (b * a)

distributiveLaws ::
  forall at t. Arbitrary at => Semiring t => Show t => SuiteEqParams1 at t -> TestSuite
distributiveLaws p =
  suite (p.suitePrefix <> " satisfies semiring distributive laws") do
    test
      ( "SHOULD HOLD left distributivity: a * (b + c) "
          <> p.eqOpSymbol
          <> " (a * b) + (a * c) FOR ALL "
          <> p.valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = p.fromArbitraryValue aA

            b = p.fromArbitraryValue bA

            c = p.fromArbitraryValue cA

            eqOp = p.eqOpWithInput $ map show [ a, b, c ]
          in
            (a * (b + c)) `eqOp` ((a * b) + (a * c))
    test
      ( "SHOULD HOLD right distributivity: (a + b) * c "
          <> p.eqOpSymbol
          <> " (a * c) + (b * c) FOR ALL "
          <> p.valuesName
          <> " a, b, c"
      )
      $ quickCheck \aA bA cA ->
          let
            a = p.fromArbitraryValue aA

            b = p.fromArbitraryValue bA

            c = p.fromArbitraryValue cA

            eqOp = p.eqOpWithInput $ map show [ a, b, c ]
          in
            ((a + b) * c) `eqOp` ((a * c) + (b * c))

otherLaws ::
  forall at t. Arbitrary at => CommutativeRing t => Show t => SuiteEqParams1 at t -> TestSuite
otherLaws p =
  suite (p.suitePrefix <> " satisfies other ring laws") do
    test
      ( "SHOULD HOLD additive inverse: a - a "
          <> p.eqOpSymbol
          <> " (zero - a) + a "
          <> p.eqOpSymbol
          <> " zero "
          <> "FOR ALL "
          <> p.valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = p.fromArbitraryValue aA

            eqOp = p.eqOpWithInput [ show a ]
          in
            ((a - a) `eqOp` zero)
              &=& (((zero - a) + a) `eqOp` zero)
    test
      ( "SHOULD HOLD annihilation: zero * a "
          <> p.eqOpSymbol
          <> " zero "
          <> "FOR ALL "
          <> p.valuesName
          <> " a"
      )
      $ quickCheck \aA ->
          let
            a = p.fromArbitraryValue aA

            eqOp = p.eqOpWithInput [ show a ]
          in
            (zero * a) `eqOp` zero

module IntervalArith.Misc where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Monoid (power)
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.String.CodeUnits as StrCU
import Data.Tuple (Tuple(..))
import FFI.BigInt (bitLength)

{-- Strings --}
fromCA :: Array Char -> String
fromCA = StrCU.fromCharArray

{-- Constants --}

two :: forall t. Semiring t => t
two = one + one

{-- Power operator --}
infixl 8 multiplicativePower as ^

infixl 8 multiplicativePowerRecip as ^^

multiplicativePower :: forall t. Semiring t => t -> Int -> t
multiplicativePower base exponent = case power (Multiplicative base) exponent of Multiplicative result -> result

multiplicativePowerRecip :: forall t. Semiring t => DivisionRing t => t -> Int -> t
multiplicativePowerRecip base exponent
  | exponent >= 0 = multiplicativePower base exponent
  | otherwise = multiplicativePower (recip base) (-exponent)

{-- Integer --}
type Integer
  = BigInt

big :: Int -> Integer
big = BigInt.fromInt

shift :: Integer -> Int -> Integer
shift x n = BigInt.shl x (Int.toNumber n)

bit :: Int -> Integer
bit n = shift (big 1) n

-- | testBit b n is true iff the n'th bit of of b is 1.
testBit :: Integer -> Int -> Boolean
testBit b n = (BigInt.and b (bit n)) /= zero

integerLog2 :: Integer -> Int
integerLog2 n = bitLength n - 1

divMod :: forall a. EuclideanRing a => a -> a -> Tuple a a
divMod a b = Tuple (div a b) (mod a b)

{-- Rational --}
type Rational
  = Ratio BigInt

class ToRational a where
  toRational :: a -> Rational

instance toRationalInt :: ToRational Int where
  toRational n = (big n) % (big 1)

instance toRationalInteger :: ToRational BigInt where
  toRational n = n % (big 1)

rationalToNumber :: Rational -> Number
rationalToNumber q = (BigInt.toNumber (numerator q)) / (BigInt.toNumber (denominator q))

roundRational :: Rational -> Integer
roundRational r =
  let
    p = numerator r

    q = denominator r

    p2 = shift p 1

    q2 = shift q 1
  in
    (p2 + q) `div` q2

ceilingRational :: Rational -> Integer
ceilingRational r =
  let
    p = numerator r

    q = denominator r
  in
    (p + q - one) `div` q

{-- Scalable --}
-- | 'Scalable' allows scaling numerical data types by powers of 2.
class Scalable a where
  scale :: a -> Int -> a

-- | The 'Integer' instance.
instance scalableInteger :: Scalable BigInt where
  scale x n
    | n >= 0 = shift x n
    | otherwise = shift (x + (bit (-n - 1))) n

-- | The 'Rational' instance.
instance scalableRational :: Scalable (Ratio BigInt) where
  scale q n = result
    where
    a = numerator q

    b = denominator q

    result
      | n >= 0 = shift a n % b
      | otherwise = a % shift b (-n)

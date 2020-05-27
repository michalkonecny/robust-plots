module IntervalArith.Misc where

import Prelude
import Data.BigInt (BigInt, fromInt, shl)
import Data.Int as Int
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.String.CodeUnits as StrCU

type Integer
  = BigInt

big :: Int -> Integer
big = fromInt

shift :: Integer -> Int -> Integer
shift x n = shl x (Int.toNumber n)

bit :: Int -> Integer
bit n = shift (big 1) n

type Rational
  = Ratio BigInt

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

fromCA :: Array Char -> String
fromCA = StrCU.fromCharArray

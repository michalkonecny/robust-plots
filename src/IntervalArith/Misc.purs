module IntervalArith.Misc where

import Data.BigInt (BigInt, fromInt, shl)
import Data.Int as Int
import Data.String.CodeUnits as StrCU

type Integer
  = BigInt

big :: Int -> Integer
big = fromInt

shift :: BigInt -> Int -> BigInt
shift x n = shl x (Int.toNumber n)

fromCA :: Array Char -> String
fromCA = StrCU.fromCharArray


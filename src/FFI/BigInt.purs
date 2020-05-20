module FFI.BigInt where

import Data.BigInt (BigInt)

foreign import bitLength :: BigInt -> Int

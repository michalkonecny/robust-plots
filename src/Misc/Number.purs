module Misc.Number where

import Prelude

import Data.Decimal as D
import Data.Int (round, toNumber)

toSignificantDigits :: Int -> Number -> Number
toSignificantDigits digits = D.toNumber <<< (D.toSignificantDigits digits) <<< D.fromNumber

to3SignificantDigits :: Number -> Number
to3SignificantDigits = toSignificantDigits 3

showNumberOrInt :: Number -> String
showNumberOrInt numberValue =
  if numberValue == toNumber integerValue then
    show integerValue
  else
    show numberValue
  where
  integerValue = round $ numberValue

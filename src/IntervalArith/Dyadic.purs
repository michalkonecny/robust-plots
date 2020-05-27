module IntervalArith.Dyadic where

import Prelude
import IntervalArith.Misc (class Scalable, Integer, big, scale, shift)

-- |The Dyadic data type.
data Dyadic
  = Dyadic Integer Int

infix 8 Dyadic as :^

instance showDyadic :: Show Dyadic where
  show (Dyadic a s) = "(Dyadic " <> show a <> " " <> show s <> ")"

instance eqDyadic :: Eq Dyadic where
  eq (a :^ s) (b :^ t)
    | s <= t = a == shift b (t - s)
    | otherwise = shift a (s - t) == b

instance ordDyadic :: Ord Dyadic where
  compare (a :^ s) (b :^ t)
    | s <= t = compare a (shift b (t - s))
    | otherwise = compare (shift a (s - t)) b

instance scalableDyadic :: Scalable Dyadic where
  scale (a :^ s) n = a :^ (s + n)

instance semiringDyadic :: Semiring Dyadic where
  zero = (big 0 :^ 0)
  one = (big 1 :^ 0)
  add (a :^ s) (b :^ t)
    | s <= t = (a + scale b (t - s)) :^ s
    | otherwise = (scale a (s - t) + b) :^ t
  mul (a :^ s) (b :^ t) = (a * b) :^ (s + t)

instance ringDyadic :: Ring Dyadic where
  sub (a :^ s) (b :^ t)
    | s <= t = (a - scale b (t - s)) :^ s
    | otherwise = (scale a (s - t) - b) :^ t

fromInteger :: Integer -> Dyadic
fromInteger i = i :^ 0

-- toRational (a :^ s) = (toRational a)*2^^s

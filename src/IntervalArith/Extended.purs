{-
  Based on CDAR.Extended by Jens Blanck (https://github.com/jensblanck/cdar).
-}
{- |The Extended module allows real-valued numeric data types to be extended by
   positive and negative infinity.
-}
module IntervalArith.Extended (Extended(..)) where

import Prelude
import Control.Apply (lift2)

-- |Extended numbers are either finite numbers or one of the two infinities.
data Extended a
  = PosInf
  | NegInf
  | Finite a

derive instance eqExtended :: (Eq a) => Eq (Extended a)

derive instance functorExtended :: Functor Extended

instance showExtended :: Show a => Show (Extended a) where
  show PosInf = "PosInf"
  show NegInf = "NegInf"
  show (Finite a) = "Finite (" <> show a <> ")"

instance applyExtended :: Apply Extended where
  apply (Finite f) (Finite x) = Finite $ f x
  apply (Finite _) PosInf = PosInf
  apply (Finite _) NegInf = NegInf
  apply PosInf _ = PosInf
  apply NegInf _ = NegInf

instance applicativeExtended :: Applicative Extended where
  pure = Finite

instance bindExtended :: Bind Extended where
  bind (Finite x) f = f x
  bind PosInf _ = PosInf
  bind NegInf _ = NegInf

instance monadExtended :: Monad Extended

instance ordExtended :: Ord a => Ord (Extended a) where
  compare PosInf PosInf = EQ
  compare NegInf NegInf = EQ
  compare _ PosInf = LT
  compare NegInf _ = LT
  compare PosInf _ = GT
  compare _ NegInf = GT
  compare (Finite a) (Finite b) = compare a b

instance semiringExtended :: (Ord a, Semiring a) => Semiring (Extended a) where
  zero = pure zero
  one = pure one
  -- PosInf + NegInf should be undefined, but here it is the first argument
  add = lift2 add
  -- 0 * ???Inf should be undefined, but here it is PosInf
  mul a PosInf = if a < zero then NegInf else PosInf
  mul PosInf a = if a < zero then NegInf else PosInf
  mul a NegInf = if a < zero then PosInf else NegInf
  mul NegInf a = if a < zero then PosInf else NegInf
  mul a b = (*) <$> a <*> b

instance ringExtended :: (Ord a, Ring a) => Ring (Extended a) where
  sub = lift2 sub

instance commutativeringExtended :: (Ord a, CommutativeRing a) => CommutativeRing (Extended a)

--     abs = fmap abs
--     signum PosInf = Finite 1
--     signum NegInf = Finite (-1)
--     signum a = signum <$> a
--     fromInteger i = Finite $ fromInteger i
-- instance Real a => Real (Extended a) where
--     toRational (Finite x) = toRational x
--     toRational _ = undefined

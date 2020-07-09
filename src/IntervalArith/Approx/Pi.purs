{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.Pi where

import Prelude

import Data.Foldable (product, sum)
import Data.List.Lazy (List, (:))
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect.Exception.Unsafe (unsafeThrow)
import IntervalArith.Approx (Approx, Precision, boundErrorTermMB, fromDyadicMB, fromInteger, fudge, limitAndBound, precision, recipA, setMB)
import IntervalArith.Approx.Sqrt (sqrtA)
import IntervalArith.Dyadic ((:^))
import IntervalArith.Extended (Extended(..))
import IntervalArith.Misc (Integer, big, (^))
import Misc.LazyList (unsafeHead, unsafeTail, zipWith4, (!!))
import Misc.LazyList.NumberSequences (ones, onetwoetc)
import Misc.Tuple (Tuple22, tuple22)

{-|
Binary splitting summation of linearly convergent series as described in
/'Fast multiprecision evaluation of series of rational numbers'/ by B Haible
and T Papanikolaou, ANTS-III Proceedings of the Third International Symposium
on Algorithmic Number Theory Pages 338-350, 1998.

The main idea is to balance the computations so that more operations are
performed with values of similar size. Using the underlying fast
multiplication algorithms this will give performance benefits.

The algorithm parallelises well. However, a final division is needed at the
end to compute /T\/BQ/ which amount to a substantial portion of the
computation time.
-}
abpq ::
  forall a.
  Semiring a =>
  (Integer -> a) ->
  List Integer ->
  List Integer ->
  List a ->
  List a ->
  Int ->
  Int ->
  Tuple22 a a Integer a
abpq fromI as bs ps qs n1 n2 = result
  where
  n = n2 - n1

  m = (n1 + n2 + 1) `div` 2

  result
    | n == 1 = tuple22 (ps !! n1) (qs !! n1) (bs !! n1) (fromI (as !! n1) * (ps !! n1))
    | n < 6 =
      let
        as' = L.take n $ L.drop n1 as

        bs' = L.take n $ L.drop n1 bs

        ps' = L.take n $ L.drop n1 ps

        qs' = L.take n $ L.drop n1 qs

        pbs = product bs'

        bs'' = map (pbs `div` _) bs'

        ps'' = L.scanl (*) one ps'

        qs'' = L.scanr (*) one (L.snoc (unsafeTail qs') one)
      in
        tuple22
          (ps'' !! (n - 1))
          (product qs')
          pbs
          ( sum
              $ zipWith4 (\a b p q -> fromI a * fromI b * p * q)
                  as'
                  bs''
                  ps''
                  qs''
          )
    | n > 1 =
      let
        (Tuple (Tuple pl ql) (Tuple bl tl)) = abpq fromI as bs ps qs n1 m

        (Tuple (Tuple pr qr) (Tuple br tr)) = abpq fromI as bs ps qs m n2
      in
        tuple22 (pl * pr) (ql * qr) (bl * br) (fromI br * qr * tl + fromI bl * pl * tr)
    | otherwise = unsafeThrow "Non-expected case in binary splitting"

{-|
Computes a sequence of approximations of π using binary splitting summation of
Ramanujan's series. See Haible and Papanikolaou 1998.
-}
piRaw :: List Approx
piRaw = unfoldr f (Tuple 1 (Tuple (Tuple one one) (Tuple one (big 13591409))))
  where
  as = L.iterate (_ + (big 545140134)) (big 13591409)

  bs = ones

  ps = one : map (\n -> -((big 6) * n - (big 5)) * ((big 2) * n - one) * ((big 6) * n - one)) onetwoetc

  qs = one : map (\n -> n ^ 3 * (big 640320) ^ 2 * (big 26680)) onetwoetc

  f (Tuple i (Tuple (Tuple pl ql) (Tuple bl tl))) =
    let
      i2 = i * 2

      Tuple (Tuple pr qr) (Tuple br tr) = abpq (\n -> n) as bs ps qs i i2

      n = 21 + 47 * (i - 1)

      x = (fromInteger tl) * recipA (setMB n $ fromInteger (bl * ql))

      x1 = fudge x $ fromDyadicMB n (one :^ (-n))

      mx2 = sqrtA (setMB n (fromInteger $ (big 1780445778) * (big 1024) * (big 1000))) -- 1823176476672000

      x3 = case mx2 of
        Just x2 -> boundErrorTermMB x2 * (recipA x1)
        Nothing -> unsafeThrow "piRaw: bad argument to sqrtA"
    in
      Just $ Tuple x3
        $ Tuple i2
        $ Tuple
            (Tuple (pl * pr) (ql * qr))
            (Tuple (bl * br) ((br * qr * tl) + (bl * pl * tr)))

-- | Computes an 'Approx' of π of the given precision.
piA :: Precision -> Approx
piA res = limitAndBound res <<< unsafeHead $ L.dropWhile ((_ < Finite res) <<< precision) piRaw

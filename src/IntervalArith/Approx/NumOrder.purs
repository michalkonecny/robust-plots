{-
  Based on CDAR.Approx fork by Michal Konečný (https://github.com/michalkonecny/cdar)
  originally developed by Jens Blanck (https://github.com/jensblanck/cdar).
-}
module IntervalArith.Approx.NumOrder where

import Prelude
import Data.BigInt (abs)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import IntervalArith.Approx (Approx(..))
import IntervalArith.Dyadic (absD, (:^))

compareA :: Approx -> Approx -> Maybe Ordering
compareA (Approx _ m e s) (Approx _ n f t)
  | e == zero && f == zero =
    pure
      $ compare (m :^ s) (n :^ t)
  | absD ((m :^ s) - (n :^ t)) > (e :^ s) + (f :^ t) =
    pure
      $ compare (m :^ s) (n :^ t)
  | otherwise = Nothing

compareA _ _ = Nothing

ltA :: Approx -> Approx -> Maybe Boolean
ltA a b = (_ == LT) <$> compareA a b

leqA :: Approx -> Approx -> Maybe Boolean
leqA a b = (_ `elem` [ LT, EQ ]) <$> compareA a b

gtA :: Approx -> Approx -> Maybe Boolean
gtA a b = (_ == GT) <$> compareA a b

geqA :: Approx -> Approx -> Maybe Boolean
geqA a b = (_ `elem` [ GT, EQ ]) <$> compareA a b

leqSureA :: Approx -> Approx -> Boolean
leqSureA a b = leqA a b == Just true

leqMaybeA :: Approx -> Approx -> Boolean
leqMaybeA a b = leqA a b /= Just false

ltSureA :: Approx -> Approx -> Boolean
ltSureA a b = ltA a b == Just true

ltMaybeA :: Approx -> Approx -> Boolean
ltMaybeA a b = ltA a b /= Just false

geqSureA :: Approx -> Approx -> Boolean
geqSureA a b = geqA a b == Just true

geqMaybeA :: Approx -> Approx -> Boolean
geqMaybeA a b = geqA a b /= Just false

gtSureA :: Approx -> Approx -> Boolean
gtSureA a b = gtA a b == Just true

gtMaybeA :: Approx -> Approx -> Boolean
gtMaybeA a b = gtA a b /= Just false

infix 4 leqSureA as !<=!

infix 4 leqMaybeA as ?<=?

infix 4 ltSureA as !<!

infix 4 ltMaybeA as ?<?

infix 4 geqSureA as !>=!

infix 4 geqMaybeA as ?>=?

infix 4 gtSureA as !>!

infix 4 gtMaybeA as ?>?

absA :: Approx -> Approx
absA (Approx mb m e s) = result
  where
  result
    | m' < e =
      let
        e' = m' + e
      in
        Approx mb e' e' (s - 1)
    | otherwise = Approx mb m' e s

  m' = abs m

absA Bottom = Bottom

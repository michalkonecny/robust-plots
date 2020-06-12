module Expression.Helper where

import Prelude
import Data.Array (fromFoldable, mapWithIndex) as A
import Data.Set (Set)
import Data.Map (Map, fromFoldable, toUnfoldable) as M
import Data.Tuple (Tuple)

setToMapWithIndex :: forall v k. Ord k => (Int -> k -> Tuple k v) -> Set k -> M.Map k v
setToMapWithIndex op = M.fromFoldable <<< (A.mapWithIndex op) <<< A.fromFoldable

mapMapEntries :: forall k v. Ord k => ((Tuple k v) -> (Tuple k v)) -> M.Map k v -> M.Map k v
mapMapEntries op = M.fromFoldable <<< mapArray <<< M.toUnfoldable
  where
  mapArray :: Array (Tuple k v) -> Array (Tuple k v)
  mapArray = map op
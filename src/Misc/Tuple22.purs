module Misc.Tuple22 where

import Data.Tuple (Tuple(..))

type Tuple22 a b c d
  = Tuple (Tuple a b) (Tuple c d)

tuple22 ::
  forall a b c d.
  b -> a -> d -> c -> Tuple (Tuple b a) (Tuple d c)
tuple22 a b c d = Tuple (Tuple a b) (Tuple c d)

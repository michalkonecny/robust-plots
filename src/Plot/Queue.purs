module Plot.Queue where

import Prelude
import Data.List as L
import Data.Maybe (Maybe, fromMaybe)

data Queue a
  = Queue (L.List a)

-- O(n)
push :: forall a. Queue a -> a -> Queue a
push (Queue queue) elem = Queue $ L.snoc queue elem

-- O(1)
tail :: forall a. Queue a -> Queue a
tail (Queue queue) = Queue $ fromMaybe L.Nil $ L.tail queue

-- O(1)
peek :: forall a. Queue a -> Maybe a
peek (Queue queue) = L.head queue

-- O(1)
null :: forall a. Queue a -> Boolean
null (Queue queue) = L.null queue

-- O(1)
toList :: forall a. Queue a -> L.List a
toList (Queue queue) = queue

-- O(1)
empty :: forall a. Queue a
empty = Queue L.Nil

-- O(n)
filter :: forall a. (a -> Boolean) -> Queue a -> Queue a
filter f (Queue queue) = Queue $ L.filter f queue

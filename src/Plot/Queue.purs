module Plot.Queue where

import Prelude
import Data.List as L
import Data.Maybe (Maybe, fromMaybe)

data Queue a
  = Queue (L.List a)

-- | Appends a element to the end of the queue
-- |
-- | Running time: `O(n)`
push :: forall a. Queue a -> a -> Queue a
push (Queue queue) elem = Queue $ L.snoc queue elem

-- | Retrives the queue without the first element. If the queue is empty, then an empty queue is returned.
-- |
-- | Running time: `O(1)`
tail :: forall a. Queue a -> Queue a
tail (Queue queue) = Queue $ fromMaybe L.Nil $ L.tail queue

-- | Retrieves the first element in the queue.
-- |
-- | Running time: `O(1)`
peek :: forall a. Queue a -> Maybe a
peek (Queue queue) = L.head queue

-- | Whether the queue has no elements or not.
-- |
-- | Running time: `O(1)`
null :: forall a. Queue a -> Boolean
null (Queue queue) = L.null queue

-- | Exposes the underlying `List`
-- |
-- | Running time: `O(1)`
toList :: forall a. Queue a -> L.List a
toList (Queue queue) = queue

-- | An empty queue
-- |
-- | Running time: `O(1)`
empty :: forall a. Queue a
empty = Queue L.Nil

-- | Partitions elements by a given predicate. 
-- |
-- | Running time: `O(n)`
partition :: forall a. (a -> Boolean) -> Queue a -> { no :: Queue a, yes :: Queue a }
partition f (Queue queue) = { no: Queue noList, yes: Queue yesList }
  where
  { no: noList, yes: yesList } = L.partition f queue

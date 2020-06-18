module Plot.Queue where

import Prelude
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (joinWith)

data Queue a
  = Queue (Array a)

push :: forall a. Queue a -> a -> Queue a
push (Queue queue) elem = Queue $ queue <> [ elem ]

pushAll :: forall a. Queue a -> Array a -> Queue a
pushAll (Queue queue) newElements = Queue $ queue <> newElements

queueTail :: forall a. Queue a -> Queue a
queueTail (Queue queue) = Queue $ fromMaybe [] $ A.tail queue

length :: forall a. Queue a -> Int
length (Queue queue) = A.length queue

peek :: forall a. Queue a -> Maybe a
peek (Queue queue) = A.head queue

null :: forall a. Queue a -> Boolean
null (Queue queue) = A.null queue

fromFoldable :: forall f. Foldable f => f ~> Queue
fromFoldable = Queue <<< A.fromFoldable

mapToArray :: forall a b. (a -> b) -> Queue a -> Array b
mapToArray f (Queue queue) = map f queue

empty :: forall a. Queue a
empty = Queue []

filter :: forall a. (a -> Boolean) -> Queue a -> Queue a
filter f (Queue queue) = Queue $ A.filter f queue

showWith :: forall a. (a -> String) -> Queue a -> String
showWith mapper (Queue queue) = "[" <> (joinWith "," (map mapper queue)) <> "]"

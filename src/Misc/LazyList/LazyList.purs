module Misc.LazyList where

import Data.List.Lazy (List, Step(..))
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import Halogen.HTML (a)
import Web.HTML.Event.EventTypes (offline)

unsafeIndex :: forall a. List a -> Int -> a
unsafeIndex list i = case list L.!! i of
  Just a -> a
  Nothing -> unsafeThrow "unsafeIndex: index out of bounds"

infixl 8 unsafeIndex as !!

unsafeHead :: forall a. List a -> a
unsafeHead list = case L.head list of
  Just a -> a
  Nothing -> unsafeThrow "unsafeHead: empty list"

unsafeTail :: forall a. List a -> List a
unsafeTail list = case L.tail list of
  Just as -> as
  Nothing -> unsafeThrow "unsafeTail: empty list"

data EmptySingletonMore a =
  Empty | Singleton a | More a a (List a)

emptySingletonMore :: forall a. List a -> EmptySingletonMore a
emptySingletonMore xs =
  case L.step xs of
    Nil -> Empty
    Cons x1 xs1 ->
      case L.step xs1 of
        Nil -> Singleton x1
        (Cons x2 rest) -> More x1 x2 rest

zipWith4 ::
  forall a b c d e.
  (a -> b -> c -> d -> e) ->
  List a -> List b -> List c -> List d -> List e
zipWith4 f l1 l2 l3 l4 =
  L.zipWith
    (\(Tuple a1 a2) (Tuple a3 a4) -> f a1 a2 a3 a4)
    (L.zip l1 l2)
    (L.zip l3 l4)


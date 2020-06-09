module Expression.VariableMap where

import Prelude
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Syntax (VariableName)

type VariableMap a
  = Array (Tuple VariableName a)

lookup :: forall a. VariableMap a -> VariableName -> Maybe a
lookup variableMap variableName = toMaybeValue $ find search variableMap
  where
  search :: Tuple VariableName a -> Boolean
  search (Tuple name _) = name == variableName

  toMaybeValue :: Maybe (Tuple VariableName a) -> Maybe a
  toMaybeValue (Just (Tuple _ value)) = Just value

  toMaybeValue _ = Nothing

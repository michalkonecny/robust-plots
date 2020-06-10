module Expression.VariableMap where

import Prelude
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Expression.Syntax (VariableName)

type VariableMap a
  = Array (Tuple VariableName a)

lookup :: forall a. VariableMap a -> VariableName -> Maybe a
lookup variableMap variableName = toMaybeValue $ find isThisVariable variableMap
  where
  isThisVariable :: Tuple VariableName a -> Boolean
  isThisVariable (Tuple name _) = name == variableName

  toMaybeValue :: Maybe (Tuple VariableName a) -> Maybe a
  toMaybeValue (Just (Tuple _ value)) = Just value

  toMaybeValue _ = Nothing

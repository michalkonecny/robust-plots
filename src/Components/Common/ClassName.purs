module Components.Common.ClassName where

import Prelude
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP

-- | Wrapper for the class name of a Halogen element.
className :: forall i r. String -> IProp ( class :: String | r ) i
className name = HP.class_ $ ClassName name

-- | Helper function for defining the class name of a Halogen element where the 
-- | class name could either one of two class names based on the `Boolean` condition.
-- | ```purescript
-- | eitherClassName "a" _ false = className "a"
-- | eitherClassName _ "b" true = className "b"
-- | ```
eitherClassName :: forall i r. String -> String -> Boolean -> IProp ( class :: String | r ) i
eitherClassName falseName trueName condition = className $ if condition then trueName else falseName

-- | Helper function for appending a optional class name to a base class name of a Halogen 
-- | element based on the `Boolean` condition.
-- | ```purescript
-- | appendClassNameIf "a" _ false = className "a"
-- | appendClassNameIf "a" "b" true = className "a b"
-- | ```
appendClassNameIf :: forall i r. String -> String -> Boolean -> IProp ( class :: String | r ) i
appendClassNameIf base optional condition = className $ if condition then base <> " " <> optional else base

-- | Helper function for specifying an optional class name for a Halogen 
-- | element based on the `Boolean` condition.
-- | ```purescript
-- | classNameIf "a" false = className ""
-- | classNameIf "a" true = className "a"
-- | ```
classNameIf :: forall i r. String -> Boolean -> IProp ( class :: String | r ) i
classNameIf = appendClassNameIf ""

module Components.Common.Styles where

import Halogen (PropName(..))
import Halogen.HTML (IProp, prop)

style :: forall r i. String -> IProp ( style :: String | r ) i
style = prop (PropName "style")

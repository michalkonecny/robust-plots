module Components.Canvas.Renderer where

import Prelude
import Data.Maybe (Maybe)
import Types (Size)
import Effect (Effect)

type Renderer context operations
  = { init :: Size -> Effect (Maybe context)
    , render :: context -> operations -> Effect Unit
    , onResize :: Size -> context -> Effect context
    }

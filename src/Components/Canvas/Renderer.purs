module Components.Canvas.Renderer where

import Prelude
import Data.Maybe (Maybe)
import Types (Size)
import Effect (Effect)
import Graphics.Canvas (Context2D)

type Renderer operations
  = { init :: Size -> Effect (Maybe Context2D)
    , render :: Context2D -> operations -> Effect Unit
    , onResize :: Size -> Context2D -> Effect Context2D
    }

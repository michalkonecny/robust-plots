module Components.Canvas.Context where

import Prelude
import Effect (Effect)
import Graphics.Canvas (Context2D, restore, save)

type DrawContext
  = { context :: Context2D
    , canvasWidth :: Number
    , canvasHeight :: Number
    }

type DrawOperation
  = DrawContext -> Effect Unit

-- | Performs a `DrawOperation` within a local drawing context such that the context 
-- | before the operation can be restored.
withLocalDrawContext :: DrawOperation -> DrawOperation
withLocalDrawContext op drawContext = do
  save drawContext.context
  op drawContext
  restore drawContext.context

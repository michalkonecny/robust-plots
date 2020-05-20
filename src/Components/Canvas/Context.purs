module Components.Canvas.Context where

import Prelude

import Components.Canvas.Renderer (Renderer)
import Constants (canvasId)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getContext2D, restore, save, setCanvasHeight, setCanvasWidth)
import Types (Size)

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

withCanvas :: (CanvasElement -> Effect Unit) -> Effect Unit
withCanvas operation = do
  maybeCanvas <- getCanvasElementById canvasId
  -- If the canvas exists, run given operation, otherwise, throw error.
  case maybeCanvas of
    Just canvas -> operation canvas
    Nothing -> throw $ "canvas id: " <> canvasId <> " was not found."

withDrawContext :: DrawOperation -> CanvasElement -> Effect Unit
withDrawContext op canvas = do
  setCanvasWidth canvas width
  setCanvasHeight canvas height
  context <- getContext2D canvas
  op { context: context, canvasWidth: width, canvasHeight: height }
  where
  width = 800.0
  height = 500.0

draw :: DrawOperation -> Effect Unit
draw = withCanvas <<< withDrawContext

type DrawOperations = Array String

renderer :: Renderer DrawOperations
renderer =
  { init, render, onResize }
  where
    init :: Size -> Effect (Maybe Context2D)
    init size = do
      maybeCanvas <- getCanvasElementById canvasId
      -- If the canvas exists, run given operation, otherwise, throw error.
      case maybeCanvas of
        Just canvas -> do
          context <- getContext2D canvas
          pure $ Just context
        Nothing -> do
          _ <- throw $ "canvas id: " <> canvasId <> " was not found."
          pure Nothing

    render :: Context2D -> DrawOperations -> Effect Unit
    render ctx operations = traverse_ (\_ -> pure unit) operations

    onResize :: Size -> Context2D-> Effect Context2D
    onResize size ctx = pure ctx
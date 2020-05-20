module Components.Canvas.Renderer where

import Prelude
import Components.Canvas.Context (DrawContext)
import Effect (Effect)
import Types (Size)
import Constants (canvasId)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById, getContext2D)

type Renderer operations
  = { init :: Size -> Effect (Maybe DrawContext)
    , render :: DrawContext -> operations -> Effect Unit
    , onResize :: Size -> DrawContext -> Effect DrawContext
    }

type DrawOperations
  = Array String

renderer :: Renderer DrawOperations
renderer = { init, render, onResize }
  where
  init :: Size -> Effect (Maybe DrawContext)
  init size = do
    maybeCanvas <- getCanvasElementById canvasId
    -- If the canvas exists, run given operation, otherwise, throw error.
    case maybeCanvas of
      Just canvas -> do
        context <- getContext2D canvas
        pure $ Just { context: context, canvasWidth: size.width, canvasHeight: size.height }
      Nothing -> do
        _ <- throw $ "canvas id: " <> canvasId <> " was not found."
        pure Nothing

  render :: DrawContext -> DrawOperations -> Effect Unit
  render ctx operations = traverse_ (\_ -> pure unit) operations

  onResize :: Size -> DrawContext -> Effect DrawContext
  onResize size ctx = pure ctx

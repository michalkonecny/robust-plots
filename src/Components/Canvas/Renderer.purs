module Components.Canvas.Renderer where

import Prelude
import Components.Canvas.Context (DrawContext)
import Components.Canvas.Draw (drawText)
import Effect (Effect)
import Types (Size)
import Constants (canvasId)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById, getContext2D)

type Renderer commands
  = { init :: Size -> Effect (Maybe DrawContext)
    , render :: DrawContext -> commands -> Effect Unit
    , onResize :: Size -> DrawContext -> Effect DrawContext
    }

type DrawCommands
  = Array String

renderer :: Renderer DrawCommands
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

  render :: DrawContext -> DrawCommands -> Effect Unit
  render ctx commands = drawText "The canvas works" 20.0 { x: 10.0, y: 30.0 } ctx

  onResize :: Size -> DrawContext -> Effect DrawContext
  onResize size ctx = pure ctx

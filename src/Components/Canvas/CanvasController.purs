module Components.Canvas.CanvasController where

import Prelude
import Components.Canvas.Commands (DrawCommand)
import Components.Canvas.Commands.Interpreter (runDrawCommands)
import Components.Canvas.Context (DrawContext)
import Constants (canvasId)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Types (Size)

type CanvasController commands
  = { init :: Size -> Effect (Maybe DrawContext)
    , render :: DrawContext -> commands -> Effect Unit
    , onResize :: Size -> DrawContext -> Effect DrawContext
    }

canvasController :: CanvasController (DrawCommand Unit)
canvasController = { init, render, onResize }
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

  render :: DrawContext -> DrawCommand Unit -> Effect Unit
  render = runDrawCommands

  onResize :: Size -> DrawContext -> Effect DrawContext
  onResize size ctx = pure ctx

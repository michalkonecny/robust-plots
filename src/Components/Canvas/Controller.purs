module Components.Canvas.Controller where

import Prelude

import Components.Canvas.Context (DrawContext)
import Components.Canvas.Interpreter (runDrawCommands)
import Constants (canvasId)
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import IntervalArith.Misc (rationalToNumber)
import Types (Size)

type CanvasController commands
  = { init :: Size -> Effect (Maybe DrawContext)
    , render :: DrawContext -> commands -> Effect Unit
    , resize :: Size -> DrawContext -> Effect DrawContext
    }

canvasController :: CanvasController (DrawCommand Unit)
canvasController = { init, render, resize }
  where
  init :: Size -> Effect (Maybe DrawContext)
  init size = do
    maybeCanvas <- getCanvasElementById canvasId
    -- If the canvas exists, run given operation, otherwise, throw error.
    case maybeCanvas of
      Just canvas -> do
        context <- getContext2D canvas
        pure $ Just { context: context, canvasWidth: rationalToNumber size.width, canvasHeight: rationalToNumber size.height }
      Nothing -> do
        _ <- throw $ "canvas id: " <> canvasId <> " was not found."
        pure Nothing

  render :: DrawContext -> DrawCommand Unit -> Effect Unit
  render = runDrawCommands

  resize :: Size -> DrawContext -> Effect DrawContext
  resize size ctx = pure $ ctx { canvasWidth = rationalToNumber size.width, canvasHeight = rationalToNumber size.height }

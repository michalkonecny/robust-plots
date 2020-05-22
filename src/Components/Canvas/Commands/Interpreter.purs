module Components.Canvas.Commands.Interpreter where

import Prelude
import Components.Canvas.Commands (DrawCommand, DrawCommandF(..))
import Components.Canvas.Context (DrawContext)
import Control.Monad.Free (foldFree)
import Effect (Effect)
import Components.Canvas.Draw (clearCanvas, drawText, drawGridLine, drawPolygon, drawEnclosure, drawRootEnclosure)

runDrawCommands :: forall a. DrawContext -> DrawCommand a -> Effect a
runDrawCommands drawContext = foldFree interpret
  where
  interpret :: DrawCommandF ~> Effect
  interpret (ClearCanvas n) = const n <$> clearCanvas drawContext

  interpret (DrawText text size pos n) = const n <$> drawText text size pos drawContext

  interpret (DrawGridLine pos1 pos2 n) = const n <$> drawGridLine pos1 pos2 drawContext

  interpret (DrawPolygon polygon n) = const n <$> drawPolygon polygon drawContext

  interpret (DrawEnclosure isSelected polygons n) = const n <$> drawEnclosure isSelected polygons drawContext

  interpret (DrawRootEnclosure yZero l r n) = const n <$> drawRootEnclosure yZero l r drawContext

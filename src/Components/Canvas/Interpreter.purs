module Components.Canvas.Interpreter where

import Prelude
import Draw.Commands (DrawCommand, DrawCommandF(..))
import Components.Canvas.Context (DrawContext)
import Control.Monad.Free (foldFree)
import Effect (Effect)
import Components.Canvas.Draw (clearCanvas, drawText, drawYGridLine, drawXGridLine, drawPolygon, drawEnclosure, drawRootEnclosure, drawPlotLine)

runDrawCommands :: forall a. DrawContext -> DrawCommand a -> Effect a
runDrawCommands drawContext = foldFree interpret
  where
  interpret :: DrawCommandF ~> Effect
  interpret (ClearCanvas n) = const n <$> clearCanvas drawContext

  interpret (DrawText text size pos n) = const n <$> drawText text size pos drawContext

  interpret (DrawXGridLine x value range n) = const n <$> drawXGridLine x value range drawContext

  interpret (DrawYGridLine y value range n) = const n <$> drawYGridLine y value range drawContext

  interpret (DrawPolygon polygon n) = const n <$> drawPolygon polygon drawContext

  interpret (DrawEnclosure isSelected polygons n) = const n <$> drawEnclosure isSelected polygons drawContext

  interpret (DrawRootEnclosure yZero l r n) = const n <$> drawRootEnclosure yZero l r drawContext

  interpret (DrawPlotLine a b n) = const n <$> drawPlotLine a b drawContext

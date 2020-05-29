module Components.Canvas.Interpreter where

import Prelude

import Components.Canvas.Context (DrawContext)
import Components.Canvas.Draw (clearCanvas, drawEnclosure, drawPlotLine, drawPolygon, drawRootEnclosure, drawText, drawXAxisLine, drawXGridLine, drawYAxisLine, drawYGridLine)
import Control.Monad.Free (foldFree)
import Draw.Commands (DrawCommand, DrawCommandF(..))
import Effect (Effect)

runDrawCommands :: forall a. DrawContext -> DrawCommand a -> Effect a
runDrawCommands drawContext = foldFree interpret
  where
  interpret :: DrawCommandF ~> Effect
  interpret (ClearCanvas n) = const n <$> clearCanvas drawContext

  interpret (DrawText text size pos n) = const n <$> drawText text size pos drawContext

  interpret (DrawXGridLine x value range n) = const n <$> drawXGridLine x value range drawContext

  interpret (DrawYGridLine y value range n) = const n <$> drawYGridLine y value range drawContext

  interpret (DrawXAxis xZero range n) = const n <$> drawXAxisLine xZero range drawContext

  interpret (DrawYAxis yZero range n) = const n <$> drawYAxisLine yZero range drawContext

  interpret (DrawPolygon polygon n) = const n <$> drawPolygon polygon drawContext

  interpret (DrawEnclosure isSelected polygons n) = const n <$> drawEnclosure isSelected polygons drawContext

  interpret (DrawRootEnclosure yZero l r n) = const n <$> drawRootEnclosure yZero l r drawContext

  interpret (DrawPlotLine a b n) = const n <$> drawPlotLine a b drawContext

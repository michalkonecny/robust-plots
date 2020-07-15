module Components.Main.Types where

import Prelude

import Components.BatchInput (BatchInputSlot)
import Components.BoundsInput (BoundsInputSlot)
import Components.Canvas (CanvasSlot, Input)
import Components.ExpressionManager (ExpressionManagerSlot)
import Components.ExpressionManager.Types (ExpressionPlot)
import Components.ProgressBar (ProgressBarSlot, Progress)
import Draw.Commands (DrawCommand)
import Types (XYBounds)

type State
  = { input :: Input (DrawCommand Unit)
    , bounds :: XYBounds
    , plots :: Array ExpressionPlot
    , clearPlot :: DrawCommand Unit
    , batchCount :: Int
    , autoRobust :: Boolean
    , progress :: Progress
    }

type ChildSlots
  = ( canvas :: CanvasSlot Int
    , expressionManager :: ExpressionManagerSlot Int
    , boundsInput :: BoundsInputSlot Int
    , batchInput :: BatchInputSlot Int
    , progressBar :: ProgressBarSlot Int
    )

type Config
  = { someData :: String }

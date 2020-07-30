module Components.Main.Types where

import Prelude

import Components.BoundsInput (BoundsInputSlot)
import Components.Canvas (CanvasSlot, Input)
import Components.ExpressionManager (ExpressionManagerSlot)
import Components.ProgressBar (ProgressBarSlot, Progress)
import Data.Maybe (Maybe)
import Draw.Commands (DrawCommand)
import Effect.Exception (Error)
import Types (XYBounds)
import ViewModels.Expression (ExpressionViewModel)

type State
  = { input :: Input (DrawCommand Unit)
    , bounds :: XYBounds
    , plots :: Array ExpressionViewModel
    , clearPlot :: DrawCommand Unit
    , batchCount :: Int
    , autoRobust :: Boolean
    , progress :: Progress
    , inProgress :: Boolean
    , error :: Maybe Error
    }

type ChildSlots
  = ( canvas :: CanvasSlot Int
    , expressionManager :: ExpressionManagerSlot Int
    , boundsInput :: BoundsInputSlot Int
    , progressBar :: ProgressBarSlot Int
    )

type Config
  = { someData :: String }

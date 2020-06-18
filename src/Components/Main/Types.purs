module Components.Main.Types where

import Prelude
import Components.Canvas (CanvasSlot, Input)
import Data.Maybe (Maybe)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Plot.JobBatcher (JobQueue)
import Types (XYBounds)
import Components.BoundsInput (BoundsInputSlot)
import Components.ExpressionInput (ExpressionInputSlot)

type State
  = { input :: Input (DrawCommand Unit)
    , bounds :: XYBounds
    , plots :: Array ExpressionPlot
    , commandSetId :: Int
    , clearPlot :: DrawCommand Unit
    , queue :: JobQueue
    }

type ExpressionPlot
  = { expression :: Maybe Expression
    , expressionText :: String
    , drawCommands :: DrawCommand Unit
    , id :: Int
    }

type ChildSlots
  = ( canvas :: CanvasSlot Int
    , expressionInput :: ExpressionInputSlot Int
    , boundsInput :: BoundsInputSlot Int
    )

type Config
  = { someData :: String }
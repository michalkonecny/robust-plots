module Components.Main.Types where

import Prelude

import Components.BatchInput (BatchInputSlot)
import Components.BoundsInput (BoundsInputSlot)
import Components.Canvas (CanvasSlot, Input)
import Components.ExpressionInput (ExpressionInputSlot)
import Data.Maybe (Maybe)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Plot.JobBatcher (JobQueue)
import Types (XYBounds)

type State
  = { input :: Input (DrawCommand Unit)
    , bounds :: XYBounds
    , plots :: Array ExpressionPlot
    , commandSetId :: Int
    , clearPlot :: DrawCommand Unit
    , queue :: JobQueue
    , batchCount :: Int
    , segmentCount :: Int
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
    , batchInput :: BatchInputSlot Int
    )

type Config
  = { someData :: String }
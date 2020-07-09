module Components.ExpressionManager.Types where

import Prelude
import Components.Checkbox (CheckboxSlot)
import Components.ExpressionInput (ExpressionInputSlot, Status)
import Data.Maybe (Maybe)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Plot.JobBatcher (JobQueue)

type ExpressionPlot
  = { expression :: Maybe Expression
    , expressionText :: String
    , commands :: ExpressionPlotCommands
    , id :: Int
    , queue :: JobQueue
    , status :: Status
    , name :: String
    , accuracy :: Number
    }

type ExpressionPlotCommands
  = { robust :: DrawCommand Unit
    , rough :: DrawCommand Unit
    , status :: DrawingStatus
    }

data DrawingStatus
  = DrawnRough
  | RobustInProgress
  | DrawnRobust
  | DrawnNone

type ChildSlots
  = ( expressionInput :: ExpressionInputSlot Int
    , checkbox :: CheckboxSlot Int
    )

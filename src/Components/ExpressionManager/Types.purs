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
    , robustDrawCommands :: DrawCommand Unit
    , roughDrawCommands :: DrawCommand Unit
    , id :: Int
    , queue :: JobQueue
    , status :: Status
    , name :: String
    , accuracy :: Number
    }

type ChildSlots
  = ( expressionInput :: ExpressionInputSlot Int
    , checkbox :: CheckboxSlot Int
    )

module Components.ExpressionManager.Types where

import Prelude
import Components.ExpressionInput (ExpressionInputSlot)
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
    , name :: String
    , accuracy :: Number
    , showRough :: Boolean
    , showRobust :: Boolean
    }

type ChildSlots
  = ( expressionInput :: ExpressionInputSlot Int
    )

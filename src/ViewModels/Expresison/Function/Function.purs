module ViewModels.Expression.Function where

import Prelude

import Data.Maybe (Maybe(..))
import Expression.Syntax (Expression)
import Plot.JobBatcher (JobQueue, initialJobQueue)
import Types (Id)
import ViewModels.Expression.Common (DrawingStatus(..), Status(..), DrawingCommands)

type FunctionViewModel
  = { expression :: Maybe Expression
    , expressionText :: String
    , commands :: DrawingCommands
    , id :: Id
    , queue :: JobQueue
    , status :: Status
    , name :: String
    , accuracy :: Number
    }

initialName :: Int -> String
initialName id = "Function " <> (show id)

newFunctionViewModel :: Id -> FunctionViewModel
newFunctionViewModel id =
  { expressionText: ""
  , expression: Nothing
  , id
  , commands:
      { robust: pure unit
      , rough: pure unit
      , status: DrawnNone
      }
  , queue: initialJobQueue
  , status: Robust
  , name: initialName id
  , accuracy: 2.0
  }

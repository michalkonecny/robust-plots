module ViewModels.Expression.Function where

import Prelude
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Plot.JobBatcher (JobQueue, initialJobQueue)
import Types (Id)
import ViewModels.Expression.Common (DrawingStatus(..), Status(..))

type FunctionViewModel
  = { expression :: Maybe Expression
    , expressionText :: String
    , commands ::
        { robust :: DrawCommand Unit
        , rough :: DrawCommand Unit
        , status :: DrawingStatus
        }
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

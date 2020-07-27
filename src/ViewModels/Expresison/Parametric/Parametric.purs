module ViewModels.Expression.Parametric where

import Prelude
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Plot.JobBatcher (JobQueue, initialJobQueue)
import Types (Id, Bounds)
import ViewModels.Expression.Common (DrawingStatus(..), Status(..))

type ParametricViewModel
  = { expression ::
        Maybe
          { x :: Expression
          , y :: Expression
          }
    , xExpressionText :: String
    , yExpressionText :: String
    , commands ::
        { robust :: DrawCommand Unit
        , rough :: DrawCommand Unit
        , status :: DrawingStatus
        }
    , domain :: Bounds
    , id :: Id
    , queue :: JobQueue
    , status :: Status
    , name :: String
    , accuracy :: Number
    }

initialName :: Int -> String
initialName id = "Parametric " <> (show id)

newParametricViewModel :: Id -> ParametricViewModel
newParametricViewModel id =
  { xExpressionText: ""
  , yExpressionText: ""
  , expression: Nothing
  , id
  , commands:
      { robust: pure unit
      , rough: pure unit
      , status: DrawnNone
      }
  , domain:
      { lower: -one
      , upper: one
      }
  , queue: initialJobQueue
  , status: Robust
  , name: initialName id
  , accuracy: 2.0
  }

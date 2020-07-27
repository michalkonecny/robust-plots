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
        Maybe ParametricExpression
    , text :: ParametricExpressionText
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

type ParametricExpression
  = { xExpression :: Expression
    , yExpression :: Expression
    }

type ParametricExpressionText
  = { xText :: String
    , yText :: String
    }

initialName :: Int -> String
initialName id = "Parametric " <> (show id)

newParametricViewModel :: Id -> ParametricViewModel
newParametricViewModel id =
  { text:
      { xText: ""
      , yText: ""
      }
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

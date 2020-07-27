module ViewModels.Expression.Function where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Draw.Commands (DrawCommand)
import Expression.Syntax (Expression)
import Misc.ExpectAff (ExpectAff, bindTo, pureRight)
import Plot.Commands (roughPlot)
import Plot.JobBatcher (JobQueue, addPlot, cancelAll, initialJobQueue)
import Plot.PlotController (computePlotAsync)
import Types (Size, XYBounds, Id)
import ViewModels.Expression.Common (AccuracyCalculator, DrawingStatus(..), Status(..), defaultPlotName)

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
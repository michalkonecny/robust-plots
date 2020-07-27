module ViewModels.Expression.Common where

import Prelude
import Data.Either (Either(..))
import Misc.ExpectAff (ExpectAff)

type AccuracyCalculator
  = Number -> Number

data DrawingStatus
  = DrawnRough
  | RobustInProgress
  | DrawnRobust
  | DrawnNone

derive instance drawingStatusEq :: Eq DrawingStatus

data Status
  = Off
  | Rough
  | Robust

derive instance statusEq :: Eq Status

defaultPlotName :: Int -> String
defaultPlotName id = "Plot " <> (show id)

handleError :: forall a b. ExpectAff a -> (a -> ExpectAff b) -> ExpectAff b
handleError operation onSuccess = do
  resultOrError <- operation
  case resultOrError of
    Left error -> pure $ Left error
    Right result -> onSuccess result

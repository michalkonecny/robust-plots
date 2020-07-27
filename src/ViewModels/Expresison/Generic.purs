module ViewModels.Expression.Generic where

import Prelude

import Control.Parallel (parSequence)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe)
import Misc.Array (alterWhere)
import Misc.ExpectAff (ExpectAff, ExpectArrayAff)
import Types (Id)
import ViewModels.Expression.Common (DrawingStatus, Status)
import ViewModels.Expression (ExpressionViewModel(..))

expressionId :: ExpressionViewModel -> Id
expressionId (Function vm) = vm.id

drawingStatus :: ExpressionViewModel -> DrawingStatus
drawingStatus (Function vm) = vm.commands.status

expressionStatus :: ExpressionViewModel -> Status
expressionStatus (Function vm) = vm.status

expressionName :: ExpressionViewModel -> String
expressionName (Function vm) = vm.name

expressionAccruacy :: ExpressionViewModel -> Number
expressionAccruacy (Function vm) = vm.accuracy

overwriteStatus :: Status -> ExpressionViewModel -> ExpressionViewModel
overwriteStatus status (Function vm) = Function $ vm { status = status }

overwriteName :: String -> ExpressionViewModel -> ExpressionViewModel
overwriteName name (Function vm) = Function $ vm { name = name }

findById :: Id -> Array ExpressionViewModel -> Maybe ExpressionViewModel
findById id vms = find (\vm -> id == expressionId vm) vms

alterExpression :: (ExpressionViewModel -> ExpressionViewModel) -> Id -> Array ExpressionViewModel -> Array ExpressionViewModel
alterExpression alterF id = alterWhere (\e -> id == expressionId e) alterF

alterExpressionAsync :: (ExpressionViewModel -> ExpectAff ExpressionViewModel) -> Id -> Array ExpressionViewModel -> ExpectArrayAff ExpressionViewModel
alterExpressionAsync alterF id = parSequence <<< map (\e -> if id == expressionId e then alterF e else pure $ Right e)
module ViewModels.Expression.Helper where

import Prelude
import Control.Parallel (parSequence)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Maybe (Maybe)
import Misc.Array (alterWhere)
import Misc.ExpectAff (ExpectAff, ExpectArrayAff)
import Types (Id)
import ViewModels.Expression (ExpressionViewModel(..))
import ViewModels.Expression.Common (DrawingStatus, Status)

expressionId :: ExpressionViewModel -> Id
expressionId (Function vm) = vm.id

expressionId (Parametric vm) = vm.id

drawingStatus :: ExpressionViewModel -> DrawingStatus
drawingStatus (Function vm) = vm.commands.status

drawingStatus (Parametric vm) = vm.commands.status

expressionStatus :: ExpressionViewModel -> Status
expressionStatus (Function vm) = vm.status

expressionStatus (Parametric vm) = vm.status

expressionName :: ExpressionViewModel -> String
expressionName (Function vm) = vm.name

expressionName (Parametric vm) = vm.name

expressionAccruacy :: ExpressionViewModel -> Number
expressionAccruacy (Function vm) = vm.accuracy

expressionAccruacy (Parametric vm) = vm.accuracy

isFunction :: ExpressionViewModel -> Boolean
isFunction (Function _) = true

isFunction _ = false

isParametric :: ExpressionViewModel -> Boolean
isParametric (Parametric _) = true

isParametric _ = false

isDefault :: ExpressionViewModel -> Boolean
isDefault (Function vm) = vm.expressionText == ""

isDefault (Parametric vm) = vm.text.xText == "" && vm.text.yText == ""

overwriteStatus :: Status -> ExpressionViewModel -> ExpressionViewModel
overwriteStatus status (Function vm) = Function $ vm { status = status }

overwriteStatus status (Parametric vm) = Parametric $ vm { status = status }

overwriteName :: String -> ExpressionViewModel -> ExpressionViewModel
overwriteName name (Function vm) = Function $ vm { name = name }

overwriteName name (Parametric vm) = Parametric $ vm { name = name }

findById :: Id -> Array ExpressionViewModel -> Maybe ExpressionViewModel
findById id vms = find (\vm -> id == expressionId vm) vms

alterExpression :: (ExpressionViewModel -> ExpressionViewModel) -> Id -> Array ExpressionViewModel -> Array ExpressionViewModel
alterExpression alterF id = alterWhere (\e -> id == expressionId e) alterF

alterExpressionAsync :: (ExpressionViewModel -> ExpectAff ExpressionViewModel) -> Id -> Array ExpressionViewModel -> ExpectArrayAff ExpressionViewModel
alterExpressionAsync alterF id = parSequence <<< map (\e -> if id == expressionId e then alterF e else pure $ Right e)

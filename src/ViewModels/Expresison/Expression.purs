module ViewModels.Expression where

import Prelude
import Types (Id)
import ViewModels.Expression.Function (FunctionViewModel, newFunctionViewModel)

data ExpressionViewModel
  = Function FunctionViewModel

newFunctionExpressionViewModel :: Id -> ExpressionViewModel
newFunctionExpressionViewModel = Function <<< newFunctionViewModel

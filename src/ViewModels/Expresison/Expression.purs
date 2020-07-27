module ViewModels.Expression where

import Prelude

import Types (Id)
import ViewModels.Expression.Function (FunctionViewModel, newFunctionViewModel)
import ViewModels.Expression.Parametric (ParametricViewModel, newParametricViewModel)

data ExpressionViewModel
  = Function FunctionViewModel
  | Parametric ParametricViewModel

newFunctionExpressionViewModel :: Id -> ExpressionViewModel
newFunctionExpressionViewModel = Function <<< newFunctionViewModel

newParametricExpressionViewModel :: Id -> ExpressionViewModel
newParametricExpressionViewModel = Parametric <<< newParametricViewModel

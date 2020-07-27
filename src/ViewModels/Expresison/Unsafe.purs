module ViewModels.Expression.Unsafe where

import ViewModels.Expression (ExpressionViewModel(..))

expressionText :: ExpressionViewModel -> String
expressionText (Function vm) = vm.expressionText

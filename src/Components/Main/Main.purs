module Components.Main where

import Prelude

import Components.BatchInput (batchInputComponent)
import Components.BoundsInput (boundsInputComponent)
import Components.Canvas (canvasComponent)
import Components.Canvas.Controller (canvasController)
import Components.ExpressionInput (expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Components.Main.Action (Action(..), handleAction)
import Components.Main.Helper (initialBounds, newPlot)
import Components.Main.Types (ChildSlots, Config, State)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import IntervalArith.Misc (toRational)
import Types (Direction(..))

_canvas = SProxy :: SProxy "canvas"

_expressionInput = SProxy :: SProxy "expressionInput"

_boundsInput = SProxy :: SProxy "boundsInput"

_batchInput = SProxy :: SProxy "batchInput"

mainComponent :: forall query input output. H.Component HH.HTML query input output (ReaderT Config Aff)
mainComponent =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Init
              }
    }
  where
  initialState :: State
  initialState =
    { input:
        { operations: pure unit
        , canvasId: canvasId
        , size:
            { width: toRational 800
            , height: toRational 500
            }
        }
    , bounds: initialBounds
    , plots:
        [ newPlot 1
        ]
    , clearPlot: pure unit
    , batchCount: 1
    , segmentCount : 50
    }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      $ [ HH.h1_
            [ HH.text "Robust plot" ]
        ]
      <> inputs
      <> [ HH.button
            [ HE.onClick $ toActionEvent $ AddPlot ]
            [ HH.text "Add plot" ]
        , HH.button
            [ HE.onClick $ toActionEvent Clear ]
            [ HH.text "Clear plots" ]
        , HH.slot _boundsInput 1 boundsInputComponent state.bounds (Just <<< HandleBoundsInput)
        , HH.slot _batchInput 1 batchInputComponent state.segmentCount (Just <<< HandleBatchInput)
        , HH.button
            [ HE.onClick $ toActionEvent $ ResetBounds ]
            [ HH.text "Reset" ]
        , HH.button
            [ HE.onClick $ toActionEvent $ Pan Left ]
            [ HH.text "◄" ]
        , HH.button
            [ HE.onClick $ toActionEvent $ Pan Right ]
            [ HH.text "►" ]
        , HH.button
            [ HE.onClick $ toActionEvent $ Pan Down ]
            [ HH.text "▼" ]
        , HH.button
            [ HE.onClick $ toActionEvent $ Pan Up ]
            [ HH.text "▲" ]
        , HH.button
            [ HE.onClick $ toActionEvent $ Zoom true ]
            [ HH.text "+" ]
        , HH.button
            [ HE.onClick $ toActionEvent $ Zoom false ]
            [ HH.text "-" ]
        , HH.slot _canvas 1 (canvasComponent canvasController) state.input (Just <<< HandleCanvas)
        ]
    where
    inputs = map (\plot -> HH.slot _expressionInput plot.id (expressionInputComponent expressionInputController plot.id) (Tuple plot.expressionText plot.status ) (Just <<< HandleExpressionInput)) state.plots

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

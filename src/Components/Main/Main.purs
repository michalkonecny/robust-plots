module Components.Main where

import Prelude
import Components.BoundsInput (boundsInputComponent)
import Components.Canvas (canvasComponent)
import Components.Canvas.Controller (canvasController)
import Components.ExpressionInput (expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import IntervalArith.Misc (toRational)
import Plot.JobBatcher (initialJobQueue)
import Types (Direction(..))
import Components.Main.Types (ChildSlots, Config, State)
import Components.Main.Action (Action(..), handleAction, initialBounds, newPlot)

_canvas = SProxy :: SProxy "canvas"

_expressionInput = SProxy :: SProxy "expressionInput"

_boundsInput = SProxy :: SProxy "boundsInput"

ui :: forall query input output. H.Component HH.HTML query input output (ReaderT Config Aff)
ui =
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
    , commandSetId: 0
    , clearPlot: pure unit
    , queue: initialJobQueue
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
    inputs = map (\plot -> HH.slot _expressionInput plot.id (expressionInputComponent expressionInputController plot.id) plot.expressionText (Just <<< HandleExpressionInput)) state.plots

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

mainComponent :: forall f i o. H.Component HH.HTML f i o Aff
mainComponent = H.hoist (\app -> runReaderT app initialConfig) ui

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI mainComponent unit body

module Components.Main where

import Prelude
import Components.AccuracyInput (accuracyInputComponent)
import Components.BatchInput (batchInputComponent)
import Components.BoundsInput (initialBounds, boundsInputComponent)
import Components.Canvas (canvasComponent)
import Components.Canvas.Controller (canvasController)
import Components.ExpressionInput (expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Components.Main.Action (Action(..), handleAction)
import Components.Main.Helper (newPlot)
import Components.Main.Types (ChildSlots, Config, State, ExpressionPlot)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT)
import Data.Array (find, length)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import IntervalArith.Misc (toRational)
import Types (Direction(..))

_canvas = SProxy :: SProxy "canvas"

_expressionInput = SProxy :: SProxy "expressionInput"

_boundsInput = SProxy :: SProxy "boundsInput"

_batchInput = SProxy :: SProxy "batchInput"

_accuracyInput = SProxy :: SProxy "accuracyInput"

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
        [ newPlot 0
        ]
    , clearPlot: pure unit
    , batchCount: 5
    , accuracy: 0.1
    , selectedPlotId: 0
    , nextPlotId: 1
    }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      $ [ HH.h1_
            [ HH.text "Robust plot" ]
        , HH.div
            [ HP.class_ (ClassName "container-fluid") ]
            [ HH.div
                [ HP.class_ (ClassName "row") ]
                [ HH.div
                    [ HP.class_ (ClassName "col-md-4") ]
                    [ HH.div
                        [ HP.class_ (ClassName "card") ]
                        [ HH.div
                            [ HP.class_ (ClassName "card-header") ]
                            [ HH.ul
                                [ HP.class_ (ClassName "nav nav-tabs card-header-tabs") ]
                                (map (toTab (1 < length state.plots) state.selectedPlotId) state.plots)
                            ]
                        , HH.div
                            [ HP.class_ (ClassName "card-body") ]
                            [ tabContent state.plots state.selectedPlotId
                            ]
                        , HH.div
                            [ HP.class_ (ClassName "card-footer") ]
                            [ HH.div
                                [ HP.class_ (ClassName "btn-group") ]
                                [ HH.button
                                    [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ AddPlot ]
                                    [ HH.text "Add plot" ]
                                , HH.button
                                    [ HP.class_ (ClassName "btn btn-danger"), HE.onClick $ toActionEvent Clear ]
                                    [ HH.text "Clear plots" ]
                                ]
                            ]
                        ]
                    , HH.br_
                    , HH.div
                        [ HP.class_ (ClassName "card") ]
                        [ HH.div
                            [ HP.class_ (ClassName "card-header") ]
                            [ HH.text "Advanced settings"
                            ]
                        , HH.div
                            [ HP.class_ (ClassName "card-body") ]
                            [ HH.slot _batchInput 1 batchInputComponent state.batchCount (Just <<< HandleBatchInput)
                            , HH.slot _accuracyInput 1 accuracyInputComponent state.accuracy (Just <<< HandleAccuracyInput)
                            ]
                        ]
                    ]
                , HH.div
                    [ HP.class_ (ClassName "col-md-8") ]
                    [ HH.div
                        [ HP.class_ (ClassName "card") ]
                        [ HH.div
                            [ HP.class_ (ClassName "card-header") ]
                            [ HH.div
                                [ HP.class_ (ClassName "row") ]
                                [ HH.div
                                    [ HP.class_ (ClassName "pr-2") ]
                                    [ HH.div
                                        [ HP.class_ (ClassName "btn-group pr-1") ]
                                        [ HH.button
                                            [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Pan Left ]
                                            [ HH.text "◄" ]
                                        , HH.button
                                            [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Pan Right ]
                                            [ HH.text "►" ]
                                        ]
                                    , HH.div
                                        [ HP.class_ (ClassName "btn-group pr-1") ]
                                        [ HH.button
                                            [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Pan Down ]
                                            [ HH.text "▼" ]
                                        , HH.button
                                            [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Pan Up ]
                                            [ HH.text "▲" ]
                                        ]
                                    , HH.div
                                        [ HP.class_ (ClassName "btn-group") ]
                                        [ HH.button
                                            [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Zoom true ]
                                            [ HH.text "+" ]
                                        , HH.button
                                            [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Zoom false ]
                                            [ HH.text "-" ]
                                        ]
                                    ]
                                , HH.div
                                    [ HP.class_ (ClassName "") ]
                                    [ HH.slot _boundsInput 1 boundsInputComponent state.bounds (Just <<< HandleBoundsInput)
                                    ]
                                ]
                            ]
                        , HH.div
                            [ HP.class_ (ClassName "card-body"), HP.id_ "canvasContainer" ]
                            [ HH.slot _canvas 1 (canvasComponent canvasController) state.input (Just <<< HandleCanvas)
                            ]
                        ]
                    ]
                ]
            ]
        ]

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

toTab :: forall w. Boolean -> Int -> ExpressionPlot -> HH.HTML w Action
toTab allowDelete selectedId plot =
  HH.li
    [ HP.class_ (ClassName "nav-item") ]
    [ HH.button
        [ HP.class_ (ClassName ("nav-link" <> maybeActiveClass))
        , (HE.onClick (toActionEvent (ChangeSelectedPlot plot.id)))
        ]
        ( [ HH.span
              [ HP.class_ (ClassName $ if allowDelete then "pr-2" else "") ]
              [ HH.text text ]
          ]
            <> cross
        )
    ]
  where
  maybeActiveClass = if selectedId == plot.id then " active" else ""

  text = if plot.expressionText == "" then "Plot " <> (show plot.id) else plot.expressionText

  cross =
    if allowDelete then
      [ HH.button
          [ HP.class_ (ClassName "close")
          , (HE.onClick (toActionEvent (DeletePlot plot.id)))
          ]
          [ HH.text "✕" ]
      ]
    else
      []

tabContent :: forall m. MonadEffect m => Array ExpressionPlot -> Int -> H.ComponentHTML Action ChildSlots m
tabContent [] _ = HH.text "Error: No plots"

tabContent plots selectedId = case find (\p -> p.id == selectedId) plots of
  Nothing -> HH.text $ "Error: Plot " <> (show selectedId) <> " does not exist"
  Just plot -> HH.slot _expressionInput plot.id component input toAction
    where
    component = expressionInputComponent expressionInputController plot.id

    input = Tuple plot.expressionText plot.status

    toAction = Just <<< HandleExpressionInput

module Components.Main where

import Prelude
import Components.AccuracyInput (accuracyInputComponent)
import Components.BatchInput (batchInputComponent)
import Components.BoundsInput (initialBounds, boundsInputComponent)
import Components.Canvas (canvasComponent)
import Components.Canvas.Controller (canvasController)
import Components.ExpressionManager (expressionManagerComponent)
import Components.Main.Action (Action(..), handleAction)
import Components.Main.Helper (newPlot)
import Components.Main.Types (ChildSlots, Config, State)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
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

_expressionManager = SProxy :: SProxy "expressionManager"

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
                    [ HH.slot _expressionManager 1 expressionManagerComponent state.plots (Just <<< HandleExpressionManager)
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

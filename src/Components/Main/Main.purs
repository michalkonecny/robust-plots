module Components.Main where

import Prelude
import Components.BatchInput (batchInputComponent)
import Components.BoundsInput (initialBounds, boundsInputComponent)
import Components.Canvas (canvasComponent)
import Components.Canvas.Controller (canvasController)
import Components.Common.Action (onClickActionEvent)
import Components.Common.ClassName (className)
import Components.ExpressionManager (Input, expressionManagerComponent)
import Components.Main.Action (Action(..), handleAction)
import Components.Main.Helper (isAllRobustPlotsComplete, newPlot)
import Components.Main.Types (ChildSlots, Config, State)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import IntervalArith.Misc (toRational)
import Types (Direction(..))

_canvas = SProxy :: SProxy "canvas"

_expressionManager = SProxy :: SProxy "expressionManager"

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
        [ newPlot 0
        ]
    , clearPlot: pure unit
    , batchCount: 5
    , autoRobust: false
    }

  render :: forall m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      $ [ HH.h1_
            [ HH.text "Robust plot" ]
        , HH.div
            [ className "container-fluid" ]
            [ HH.div
                [ className "row" ]
                [ HH.div
                    [ className "col-md-4" ]
                    [ HH.slot _expressionManager 1 expressionManagerComponent (toExpressionManagerInput state) (Just <<< HandleExpressionManager)
                    , HH.br_
                    , HH.div
                        [ className "card" ]
                        [ HH.div
                            [ className "card-header" ]
                            [ HH.text "Advanced settings"
                            ]
                        , HH.div
                            [ className "card-body" ]
                            [ HH.slot _batchInput 1 batchInputComponent state.batchCount (Just <<< HandleBatchInput)
                            ]
                        ]
                    ]
                , HH.div
                    [ className "col-md-8" ]
                    [ HH.div
                        [ className "card" ]
                        [ HH.div
                            [ className "card-header" ]
                            [ HH.div
                                [ className "row" ]
                                [ HH.div
                                    [ className "pr-2" ]
                                    [ HH.div
                                        [ className "btn-group pr-1" ]
                                        [ HH.button
                                            [ className "btn btn-primary"
                                            , onClickActionEvent $ Pan Left
                                            ]
                                            [ HH.text "◄" ]
                                        , HH.button
                                            [ className "btn btn-primary"
                                            , onClickActionEvent $ Pan Right
                                            ]
                                            [ HH.text "►" ]
                                        ]
                                    , HH.div
                                        [ className "btn-group pr-1" ]
                                        [ HH.button
                                            [ className "btn btn-primary"
                                            , onClickActionEvent $ Pan Down
                                            ]
                                            [ HH.text "▼" ]
                                        , HH.button
                                            [ className "btn btn-primary"
                                            , onClickActionEvent $ Pan Up
                                            ]
                                            [ HH.text "▲" ]
                                        ]
                                    , HH.div
                                        [ className "btn-group" ]
                                        [ HH.button
                                            [ className "btn btn-primary"
                                            , onClickActionEvent $ Zoom true
                                            ]
                                            [ HH.text "+" ]
                                        , HH.button
                                            [ className "btn btn-primary"
                                            , onClickActionEvent $ Zoom false
                                            ]
                                            [ HH.text "-" ]
                                        ]
                                    ]
                                , HH.div
                                    []
                                    [ HH.slot _boundsInput 1 boundsInputComponent state.bounds (Just <<< HandleBoundsInput)
                                    ]
                                ]
                            ]
                        , HH.div
                            [ className "card-body"
                            , HP.id_ "canvasContainer"
                            ]
                            [ HH.slot _canvas 1 (canvasComponent canvasController) state.input (Just <<< HandleCanvas)
                            ]
                        ]
                    ]
                ]
            ]
        ]

toExpressionManagerInput :: State -> Input
toExpressionManagerInput state = { plots: state.plots, autoRobust: state.autoRobust, allRobustDraw: isAllRobustPlotsComplete state.plots }

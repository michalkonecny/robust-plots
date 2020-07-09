module Components.ExpressionManager where

import Prelude
import Components.Checkbox (CheckboxMessage(..), checkboxComponent)
import Components.Common.Action (onClickActionEvent, onFocusOutActionEvent, onValueChangeActionEvent)
import Components.Common.ClassName (appendClassNameIf, className, classNameIf)
import Components.ExpressionInput (ExpressionInputMessage, expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Components.ExpressionManager.Types (ExpressionPlot, ChildSlots)
import Data.Array (catMaybes, find, head, length)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

_expressionInput = SProxy :: SProxy "expressionInput"

_checkbox = SProxy :: SProxy "checkbox"

type ExpressionManagerSlot p
  = forall q. H.Slot q ExpressionManagerMessage p

type State
  = { plots :: Array ExpressionPlot
    , selectedPlotId :: Int
    , nextPlotId :: Int
    , editingSelected :: Boolean
    , editedName :: String
    , autoRobust :: Boolean
    , allRobustDraw :: Boolean
    }

type Input
  = { plots :: Array ExpressionPlot
    , autoRobust :: Boolean
    , allRobustDraw :: Boolean
    }

data ExpressionManagerMessage
  = AddPlot Int
  | ClearPlots
  | DeletePlot Int
  | RenamePlot Int String
  | RaisedExpressionInputMessage ExpressionInputMessage
  | ToggleAuto Boolean
  | CalulateRobustPlots

data Action
  = HandleMessage Input
  | Clear
  | Add
  | Delete Int
  | Edit
  | Rename
  | HandleInput String
  | ChangeSelected Int
  | HandleExpressionInput ExpressionInputMessage
  | HandleAutoToggle CheckboxMessage
  | CalulateRobust

expressionManagerComponent :: forall query m. MonadEffect m => H.Component HH.HTML query Input ExpressionManagerMessage m
expressionManagerComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< HandleMessage
              }
    }

initialState :: Input -> State
initialState { autoRobust, plots, allRobustDraw } =
  { plots
  , selectedPlotId
  , editingSelected: false
  , nextPlotId: 1
  , editedName: selectedPlotName plots selectedPlotId
  , autoRobust
  , allRobustDraw
  }
  where
  selectedPlotId = firstPlotId plots

render :: forall m. MonadEffect m => State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.div
        [ className "card" ]
        [ HH.div
            [ className "card-header" ]
            [ HH.ul
                [ className "nav nav-tabs card-header-tabs" ]
                ((map (toTab state) state.plots) <> [ addPlotTab ])
            ]
        , HH.div
            [ className "card-body" ]
            [ selectedExpressionPlot state.plots state.selectedPlotId
            ]
        , HH.div
            [ className "card-footer" ]
            [ HH.div
                [ className "form-inline" ]
                [ HH.div
                    [ className "btn-group" ]
                    [ HH.button
                        [ className "btn btn-danger"
                        , onClickActionEvent Clear
                        ]
                        [ HH.text "Clear plots" ]
                    , HH.button
                        [ className "btn btn-primary"
                        , HP.disabled state.allRobustDraw
                        , onClickActionEvent CalulateRobust
                        ]
                        [ HH.text "Render Robust Plots" ]
                    ]
                , HH.div
                    [ className "pl-2" ]
                    [ HH.slot _checkbox 1 (checkboxComponent "Auto") state.autoRobust (Just <<< HandleAutoToggle) ]
                ]
            ]
        ]
    ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots ExpressionManagerMessage m Unit
handleAction = case _ of
  HandleMessage { autoRobust, plots, allRobustDraw } -> do
    { selectedPlotId } <- H.get
    let
      newSelectedPlotId =
        if plotExists plots selectedPlotId then
          selectedPlotId
        else
          firstPlotId plots
    H.modify_
      _
        { plots = plots
        , selectedPlotId = newSelectedPlotId
        , editingSelected = false
        , editedName = selectedPlotName plots newSelectedPlotId
        , autoRobust = autoRobust
        , allRobustDraw = allRobustDraw
        }
  Clear -> H.raise ClearPlots
  Add -> do
    { nextPlotId } <- H.get
    H.modify_ (_ { nextPlotId = nextPlotId + 1 })
    H.raise (AddPlot nextPlotId)
  Delete plotId -> do H.raise $ DeletePlot plotId
  Edit -> H.modify_ (_ { editingSelected = true })
  Rename -> do
    { selectedPlotId, editedName } <- H.get
    H.raise $ RenamePlot selectedPlotId editedName
  HandleInput name -> H.modify_ (_ { editedName = name })
  ChangeSelected plotId -> do
    { selectedPlotId, plots } <- H.get
    when ((plotExists plots plotId) && selectedPlotId /= plotId) do
      H.modify_ (_ { selectedPlotId = plotId, editingSelected = false, editedName = selectedPlotName plots plotId })
  HandleExpressionInput message -> H.raise $ RaisedExpressionInputMessage message
  HandleAutoToggle (ToggleChanged isChecked) -> H.raise $ ToggleAuto isChecked
  CalulateRobust -> H.raise CalulateRobustPlots

toTab :: forall w. State -> ExpressionPlot -> HH.HTML w Action
toTab state plot =
  HH.li
    [ className "nav-item" ]
    [ HH.button
        [ appendClassNameIf "nav-link" "active" $ state.selectedPlotId == plot.id
        , onClickActionEvent $ ChangeSelected plot.id
        ]
        [ HH.div
            [ className "form-inline" ]
            tabContent
        ]
    ]
  where
  tabContent = catMaybes [ Just (textOrEditInput state plot), maybeEditButton state plot.id, maybeDeleteButton state plot.id ]

addPlotTab :: forall w. HH.HTML w Action
addPlotTab =
  HH.li
    [ className "nav-item" ]
    [ HH.button
        [ className "nav-link"
        ]
        [ HH.div
            [ className "form-inline" ]
            [ HH.button
                [ className "btn-success btn-sm"
                , onClickActionEvent Add
                ]
                [ HH.text "+" ]
            ]
        ]
    ]

textOrEditInput :: forall w. State -> ExpressionPlot -> HH.HTML w Action
textOrEditInput state plot =
  if (state.selectedPlotId == plot.id) && state.editingSelected then
    HH.input
      [ HP.type_ HP.InputText
      , onFocusOutActionEvent Rename
      , onValueChangeActionEvent HandleInput
      , HP.value state.editedName
      , appendClassNameIf "form-control-sm small-input" "pr-2" addPaddingToInput
      ]
  else
    HH.span
      [ classNameIf "pr-2" addPaddingToInput ]
      [ HH.text plot.name ]
  where
  addPaddingToInput = (1 < length state.plots) || not state.editingSelected

maybeEditButton :: forall w. State -> Int -> Maybe (HH.HTML w Action)
maybeEditButton state plotId =
  if state.selectedPlotId == plotId && not state.editingSelected then
    Just
      $ HH.button
          [ className "btn-primary btn-sm"
          , onClickActionEvent Edit
          ]
          [ HH.text "🖉" ]
  else
    Nothing

maybeDeleteButton :: forall w. State -> Int -> Maybe (HH.HTML w Action)
maybeDeleteButton state plotId =
  if 1 < length state.plots then
    Just
      $ HH.button
          [ className "btn-danger btn-sm"
          , onClickActionEvent $ Delete plotId
          ]
          [ HH.text "✕" ]
  else
    Nothing

selectedExpressionPlot :: forall m. MonadEffect m => Array ExpressionPlot -> Int -> H.ComponentHTML Action ChildSlots m
selectedExpressionPlot [] _ = HH.text "Error: No plots"

selectedExpressionPlot plots selectedPlotId = case find (\p -> p.id == selectedPlotId) plots of
  Nothing -> HH.text $ "Error: Plot " <> (show selectedPlotId) <> " does not exist"
  Just plot -> HH.slot _expressionInput plot.id component input toAction
    where
    component = expressionInputComponent expressionInputController plot.id

    input = { expressionText: plot.expressionText, status: plot.status, accuracy: plot.accuracy }

    toAction = Just <<< HandleExpressionInput

plotExists :: Array ExpressionPlot -> Int -> Boolean
plotExists plots plotId = isJust (find (\p -> p.id == plotId) plots)

selectedPlotName :: Array ExpressionPlot -> Int -> String
selectedPlotName plots selectedPlotId = fromMaybe "Error" $ (_.name) <$> (find (\p -> p.id == selectedPlotId) plots)

firstPlotId :: Array ExpressionPlot -> Int
firstPlotId plots = fromMaybe 0 $ (_.id) <$> (head plots)

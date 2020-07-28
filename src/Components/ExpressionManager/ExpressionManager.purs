module Components.ExpressionManager where

import Prelude
import Components.Checkbox (CheckboxMessage(..), checkboxComponent)
import Components.Common.Action (onClickActionEvent, onEnterPressActionEvent, onFocusOutActionEvent, onSelectedIndexChangeActionEvent, onValueChangeActionEvent)
import Components.Common.ClassName (appendClassNameIf, className, classNameIf)
import Components.Common.Styles (style)
import Components.ExpressionInput (ExpressionInputMessage(..), expressionInputComponent, parseAndCheckExpression)
import Components.ExpressionInput.Controller (expressionInputController)
import Components.ExpressionManager.Types (ExpressionPlot, ChildSlots)
import Data.Array (catMaybes, find, head, length, (!!))
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA

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
    , inProgress :: Boolean
    }

type Input
  = { plots :: Array ExpressionPlot
    , autoRobust :: Boolean
    , allRobustDraw :: Boolean
    , inProgress :: Boolean
    }

data ExpressionManagerMessage
  = AddPlot Int
  | DeletePlot Int
  | RenamePlot Int String
  | RaisedExpressionInputMessage ExpressionInputMessage
  | ToggleAuto Boolean
  | CalulateRobustPlots

data Action
  = HandleMessage Input
  | Add
  | Delete Int
  | Edit
  | Rename
  | HandleInput String
  | ChangeSelected Int
  | HandleExpressionInput ExpressionInputMessage
  | HandleAutoToggle CheckboxMessage
  | CalulateRobust
  | SelectedExample Int

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
initialState { autoRobust, plots, allRobustDraw, inProgress } =
  { plots
  , selectedPlotId
  , editingSelected: false
  , nextPlotId: 1
  , editedName: selectedPlotName plots selectedPlotId
  , autoRobust
  , allRobustDraw
  , inProgress
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
                    [ renderButton state.allRobustDraw state.inProgress ]
                , HH.div
                    [ className "pl-2" ]
                    [ HH.slot _checkbox 1 (checkboxComponent "Auto") state.autoRobust (Just <<< HandleAutoToggle) ]
                ]
            ]
        ]
    ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots ExpressionManagerMessage m Unit
handleAction = case _ of
  HandleMessage { autoRobust, plots, allRobustDraw, inProgress } -> do
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
        , inProgress = inProgress
        }
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
  SelectedExample index -> handleAddExample index

handleAddExample :: forall m. MonadEffect m => Int -> H.HalogenM State Action ChildSlots ExpressionManagerMessage m Unit
handleAddExample index = do
  when (index /= 0) do
    { selectedPlotId, plots } <- H.get
    case examples !! (index - 1), plots !! selectedPlotId of
      Nothing, _ -> pure unit
      Just example, Nothing -> do
        { nextPlotId } <- H.get
        handleAction Add
        overwriteWithExample nextPlotId (NonEmptyArray.head example)
      Just example, Just selected -> do
        if selected.expressionText == "" then do
          overwriteWithExample selected.id (NonEmptyArray.head example)
        else do
          { nextPlotId } <- H.get
          handleAction Add
          overwriteWithExample nextPlotId (NonEmptyArray.head example)

overwriteWithExample :: forall m. MonadEffect m => Int -> String -> H.HalogenM State Action ChildSlots ExpressionManagerMessage m Unit
overwriteWithExample id example = case parseAndCheckExpression expressionInputController example of
  Left _ -> pure unit
  Right expression -> H.raise $ RaisedExpressionInputMessage $ ParsedExpression id (expressionInputController.clean expression) example

infix 6 cons' as :.

examples :: Array (NonEmptyArray String)
examples =
  [ "sin(15*(x^4))" :. []
  , "x*sin(4/(x^2))" :. [ "... (Continuous despite singularity)" ]
  , "max(0,x*sin(4/(x^2)))" :. [ "... (Takes minutes to compute)" ]
  , "1/(1+1000000*(x-0.5)^2)" :. []
  , "0.1+(10^20)*(x^2-1)-(10^20)*(x+1)*(x-1)" :. [ "... (Rough plot distorted)" ]
  , "(1+x^2)^(sin(10*x))-1" :. [ "... (Takes minutes to compute)" ]
  ]

exampleFunctionOptions :: forall w. Array (HH.HTML w Action)
exampleFunctionOptions = [ HH.option [ HP.disabled true, HP.selected true ] [ HH.text "Add example function from below" ] ] <> map toOption examples
  where
  toOption :: NonEmptyArray String -> HH.HTML w Action
  toOption text = HH.option_ [ HH.text (joinWith " " (NonEmptyArray.toArray text)) ]

renderButton :: forall w. Boolean -> Boolean -> HH.HTML w Action
renderButton disabled false =
  HH.button
    [ className "btn btn-primary"
    , HP.disabled disabled
    , onClickActionEvent CalulateRobust
    ]
    [ HH.text "Render Robust Plots" ]

renderButton _ true =
  HH.button
    [ className "btn btn-primary"
    , HP.disabled true
    , onClickActionEvent CalulateRobust
    ]
    [ HH.text "Render Robust Plots "
    , HH.span
        [ className "spinner-border spinner-border-sm", HA.role "status", HA.hidden "true" ]
        []
    ]

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
                [ className "btn btn-success btn-sm"
                , onClickActionEvent Add
                ]
                [ HH.text "+" ]
            , HH.select
                [ className "form-control form-control-sm"
                , style "max-width: 20px"
                , HP.selectedIndex 0
                , onSelectedIndexChangeActionEvent SelectedExample
                ]
                exampleFunctionOptions
            ]
        ]
    ]

textOrEditInput :: forall w. State -> ExpressionPlot -> HH.HTML w Action
textOrEditInput state plot =
  if (state.selectedPlotId == plot.id) && state.editingSelected then
    HH.input
      [ HP.type_ HP.InputText
      , onFocusOutActionEvent Rename
      , onEnterPressActionEvent Rename
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
          [ className "btn btn-primary btn-sm"
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
          [ className "btn btn-danger btn-sm"
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

module Components.ExpressionManager where

import Prelude

import Components.Checkbox (CheckboxSlot, CheckboxMessage(..), checkboxComponent)
import Components.Common.Action (onClickActionEvent, onEnterPressActionEvent, onFocusOutActionEvent, onSelectedIndexChangeActionEvent, onValueChangeActionEvent)
import Components.Common.ClassName (appendClassNameIf, className, classNameIf)
import Components.Common.Styles (style)
import Components.ExpressionInput.Controller (expressionInputController)
import Components.ExpressionInput.FunctionExpressionInput (FunctionExpressionInputSlot, FunctionExpressionInputMessage(..), functionExpressionInputComponent, parseAndCheckExpression)
import Components.ExpressionInput.ParametricExpressionInput (ParametricExpressionInputMessage, ParametricExpressionInputSlot, parametricExpressionInputComponent)
import Data.Array (catMaybes, head, length, (!!))
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
import ViewModels.Expression (ExpressionViewModel(..), newFunctionExpressionViewModel, newParametricExpressionViewModel)
import ViewModels.Expression.Generic (expressionId, expressionName, findById, isDefault)

_functionExpressionInput = SProxy :: SProxy "functionExpressionInput"

_parametricExpressionInput = SProxy :: SProxy "parametricExpressionInput"

_checkbox = SProxy :: SProxy "checkbox"

type ExpressionManagerSlot p
  = forall q. H.Slot q ExpressionManagerMessage p

type ChildSlots
  = ( functionExpressionInput :: FunctionExpressionInputSlot Int
    , checkbox :: CheckboxSlot Int
    , parametricExpressionInput :: ParametricExpressionInputSlot Int
    )

type State
  = { plots :: Array ExpressionViewModel
    , selectedPlotId :: Int
    , nextPlotId :: Int
    , editingSelected :: Boolean
    , editedName :: String
    , autoRobust :: Boolean
    , allRobustDraw :: Boolean
    , inProgress :: Boolean
    }

type Input
  = { plots :: Array ExpressionViewModel
    , autoRobust :: Boolean
    , allRobustDraw :: Boolean
    , inProgress :: Boolean
    }

data ExpressionManagerMessage
  = AddPlot ExpressionViewModel
  | DeletePlot Int
  | RenamePlot Int String
  | RaisedFunctionExpressionInputMessage FunctionExpressionInputMessage
  | RaisedParametricExpressionInputMessage ParametricExpressionInputMessage
  | ToggleAuto Boolean
  | CalulateRobustPlots

data Action
  = HandleMessage Input
  | AddFunction
  | AddParametric
  | Delete Int
  | Edit
  | Rename
  | HandleInput String
  | ChangeSelected Int
  | HandleFunctionExpressionInput FunctionExpressionInputMessage
  | HandleParametricExpressionInput ParametricExpressionInputMessage
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
                ((map (toTab state) state.plots) <> [ addTab ])
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
  AddFunction -> do
    { nextPlotId } <- H.get
    H.modify_ (_ { nextPlotId = nextPlotId + 1 })
    H.raise $ AddPlot $ newFunctionExpressionViewModel nextPlotId
  AddParametric -> do
    { nextPlotId } <- H.get
    H.modify_ (_ { nextPlotId = nextPlotId + 1 })
    H.raise $ AddPlot $ newParametricExpressionViewModel nextPlotId
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
  HandleFunctionExpressionInput message -> H.raise $ RaisedFunctionExpressionInputMessage message
  HandleParametricExpressionInput message -> H.raise $ RaisedParametricExpressionInputMessage message
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
        handleAction AddFunction
        overwriteWithExample nextPlotId (NonEmptyArray.head example)
      Just example, Just selected -> do
        if isDefault selected then do
          overwriteWithExample (expressionId selected) (NonEmptyArray.head example)
        else do
          { nextPlotId } <- H.get
          handleAction AddFunction
          overwriteWithExample nextPlotId (NonEmptyArray.head example)

overwriteWithExample :: forall m. MonadEffect m => Int -> String -> H.HalogenM State Action ChildSlots ExpressionManagerMessage m Unit
overwriteWithExample id example = case parseAndCheckExpression expressionInputController example of
  Left _ -> pure unit
  Right expression -> H.raise $ RaisedFunctionExpressionInputMessage $ FunctionParsedExpression id (expressionInputController.clean expression) example

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
exampleFunctionOptions = [ HH.option [ HP.disabled true, HP.selected true ] [ HH.text "AddFunction example function from below" ] ] <> map toOption examples
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

toTab :: forall w. State -> ExpressionViewModel -> HH.HTML w Action
toTab state plot =
  HH.li
    [ className "nav-item" ]
    [ HH.button
        [ appendClassNameIf "nav-link" "active" $ state.selectedPlotId == expressionId plot
        , onClickActionEvent $ ChangeSelected $ expressionId plot
        ]
        [ HH.div
            [ className "form-inline" ]
            tabContent
        ]
    ]
  where
  tabContent = catMaybes [ Just (textOrEditInput state plot), maybeEditButton state (expressionId plot), maybeDeleteButton state (expressionId plot) ]

addTab :: forall w. HH.HTML w Action
addTab =
  HH.li
    [ className "nav-item" ]
    [ HH.button
        [ className "nav-link"
        ]
        [ HH.div
            [ className "form-inline" ]
            [ HH.button
                [ className "btn btn-success btn-sm"
                , onClickActionEvent AddParametric
                ]
                [ HH.text "P" ],
            HH.button
                [ className "btn btn-success btn-sm"
                , onClickActionEvent AddFunction
                ]
                [ HH.text "F" ]
            
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


textOrEditInput :: forall w. State -> ExpressionViewModel -> HH.HTML w Action
textOrEditInput state plot =
  if (state.selectedPlotId == expressionId plot) && state.editingSelected then
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
      [ HH.text $ expressionName plot ]
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

selectedExpressionPlot :: forall m. MonadEffect m => Array ExpressionViewModel -> Int -> H.ComponentHTML Action ChildSlots m
selectedExpressionPlot [] _ = HH.text "Error: No plots"

selectedExpressionPlot plots selectedPlotId = case findById selectedPlotId plots of
  Nothing -> HH.text $ "Error: Plot " <> (show selectedPlotId) <> " does not exist"
  Just plot -> go plot
    where
    go :: ExpressionViewModel -> H.ComponentHTML Action ChildSlots m
    go (Function vm) = HH.slot _functionExpressionInput vm.id component input toAction
      where
      component = functionExpressionInputComponent expressionInputController vm.id

      input = { expressionText: vm.expressionText, status: vm.status, accuracy: vm.accuracy }

      toAction = Just <<< HandleFunctionExpressionInput

    go (Parametric vm) = HH.slot _parametricExpressionInput vm.id component input toAction
      where
      component = parametricExpressionInputComponent expressionInputController vm.id

      input = { expressionText: vm.text, status: vm.status, accuracy: vm.accuracy, domain: vm.domain }

      toAction = Just <<< HandleParametricExpressionInput

plotExists :: Array ExpressionViewModel -> Int -> Boolean
plotExists plots plotId = isJust (findById plotId plots)

selectedPlotName :: Array ExpressionViewModel -> Int -> String
selectedPlotName plots selectedPlotId = fromMaybe "Error" $ expressionName <$> (findById selectedPlotId plots)

firstPlotId :: Array ExpressionViewModel -> Int
firstPlotId plots = fromMaybe 0 $ expressionId <$> (head plots)

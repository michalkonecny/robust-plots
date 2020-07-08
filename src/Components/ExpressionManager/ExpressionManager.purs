module Components.ExpressionManager where

import Prelude
import Components.ExpressionInput (ExpressionInputMessage, expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Components.ExpressionManager.Types (ExpressionPlot, ChildSlots)
import Data.Array (catMaybes, find, head, length)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

_expressionInput = SProxy :: SProxy "expressionInput"

type ExpressionManagerSlot p
  = forall q. H.Slot q ExpressionManagerMessage p

type State
  = { plots :: Array ExpressionPlot
    , selectedPlotId :: Int
    , nextPlotId :: Int
    , editingSelected :: Boolean
    , editedName :: String
    }

data ExpressionManagerMessage
  = AddPlot Int
  | ClearPlots
  | DeletePlot Int
  | RenamePlot Int String
  | RaisedExpressionInputMessage ExpressionInputMessage

data Action
  = HandleMessage (Array ExpressionPlot)
  | Clear
  | Add
  | Delete Int
  | Edit
  | Rename
  | HandleInput String
  | ChangeSelected Int
  | HandleExpressionInput ExpressionInputMessage

expressionManagerComponent :: forall query m. MonadEffect m => H.Component HH.HTML query (Array ExpressionPlot) ExpressionManagerMessage m
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

initialState :: Array ExpressionPlot -> State
initialState plots =
  { plots
  , selectedPlotId
  , editingSelected: false
  , nextPlotId: 1
  , editedName: selectedPlotName plots selectedPlotId
  }
  where
  selectedPlotId = firstPlotId plots

render :: forall m. MonadEffect m => State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.div
        [ HP.class_ (ClassName "card") ]
        [ HH.div
            [ HP.class_ (ClassName "card-header") ]
            [ HH.ul
                [ HP.class_ (ClassName "nav nav-tabs card-header-tabs") ]
                (map (toTab state) state.plots)
            ]
        , HH.div
            [ HP.class_ (ClassName "card-body") ]
            [ selectedExpressionPlot state.plots state.selectedPlotId
            ]
        , HH.div
            [ HP.class_ (ClassName "card-footer") ]
            [ HH.div
                [ HP.class_ (ClassName "btn-group") ]
                [ HH.button
                    [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Add ]
                    [ HH.text "Add plot" ]
                , HH.button
                    [ HP.class_ (ClassName "btn btn-danger"), HE.onClick $ toActionEvent Clear ]
                    [ HH.text "Clear plots" ]
                ]
            ]
        ]
    ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots ExpressionManagerMessage m Unit
handleAction = case _ of
  HandleMessage plots -> do
    { selectedPlotId } <- H.get
    let
      newSelectedPlotId =
        if plotExists plots selectedPlotId then
          selectedPlotId
        else
          firstPlotId plots
    H.modify_ _ { plots = plots, selectedPlotId = newSelectedPlotId, editingSelected = false, editedName = selectedPlotName plots newSelectedPlotId }
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

toTab :: forall w. State -> ExpressionPlot -> HH.HTML w Action
toTab state plot =
  HH.li
    [ HP.class_ (ClassName "nav-item") ]
    [ HH.button
        [ HP.class_ $ ClassName className
        , HE.onClick $ toActionEvent (ChangeSelected plot.id)
        ]
        [ HH.div
            [ HP.class_ (ClassName "form-inline") ]
            tabContent
        ]
    ]
  where
  className = if state.selectedPlotId == plot.id then "nav-link active" else "nav-link"

  tabContent = catMaybes [ Just (textOrEditInput state plot), maybeEditButton state plot.id, maybeDeleteButton state plot.id ]

textOrEditInput :: forall w. State -> ExpressionPlot -> HH.HTML w Action
textOrEditInput state plot =
  if (state.selectedPlotId == plot.id) && state.editingSelected then
    HH.input
      [ HP.type_ HP.InputText
      , HE.onFocusOut $ toActionEvent Rename
      , HE.onValueChange $ toValueChangeActionEvent
      , HP.value state.editedName
      , HP.class_ (ClassName $ if addPaddingToInput then "form-control-sm small-input pr-2" else "form-control small-input")
      ]
  else
    HH.span
      [ HP.class_ (ClassName $ if addPaddingToInput then "pr-2" else "") ]
      [ HH.text plot.name ]
  where
  addPaddingToInput = (1 < length state.plots) || not state.editingSelected

maybeEditButton :: forall w. State -> Int -> Maybe (HH.HTML w Action)
maybeEditButton state plotId =
  if state.selectedPlotId == plotId && not state.editingSelected then
    Just
      $ HH.button
          [ HP.class_ (ClassName "btn-primary btn-sm")
          , HE.onClick $ toActionEvent Edit
          ]
          [ HH.text "🖉" ]
  else
    Nothing

maybeDeleteButton :: forall w. State -> Int -> Maybe (HH.HTML w Action)
maybeDeleteButton state plotId =
  if 1 < length state.plots then
    Just
      $ HH.button
          [ HP.class_ (ClassName "btn-danger btn-sm")
          , HE.onClick $ toActionEvent (Delete plotId)
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

    input = Tuple plot.expressionText plot.status

    toAction = Just <<< HandleExpressionInput

plotExists :: Array ExpressionPlot -> Int -> Boolean
plotExists plots plotId = isJust (find (\p -> p.id == plotId) plots)

selectedPlotName :: Array ExpressionPlot -> Int -> String
selectedPlotName plots selectedPlotId = fromMaybe "Error" $ (_.name) <$> (find (\p -> p.id == selectedPlotId) plots)

toValueChangeActionEvent :: String -> Maybe Action
toValueChangeActionEvent value = Just $ HandleInput value

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

firstPlotId :: Array ExpressionPlot -> Int
firstPlotId plots = fromMaybe 0 $ (_.id) <$> (head plots)

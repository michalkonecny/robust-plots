module Components.ExpressionManager where

import Prelude
import Components.ExpressionInput (ExpressionInputMessage, expressionInputComponent)
import Components.ExpressionInput.Controller (expressionInputController)
import Components.ExpressionManager.Types (ExpressionPlot, ChildSlots)
import Data.Array (filter, find, head, length)
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
  , selectedPlotId: fromMaybe 0 $ (_.id) <$> (head plots)
  , editingSelected: false
  , nextPlotId: 1
  }

render :: forall m. MonadEffect m => State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.div
        [ HP.class_ (ClassName "card") ]
        [ HH.div
            [ HP.class_ (ClassName "card-header") ]
            [ HH.ul
                [ HP.class_ (ClassName "nav nav-tabs card-header-tabs") ]
                (map (toTab (1 < length state.plots) state.editingSelected state.selectedPlotId) state.plots)
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
                    [ HP.class_ (ClassName "btn btn-primary"), HE.onClick $ toActionEvent $ Add ]
                    [ HH.text "Add plot" ]
                , HH.button
                    [ HP.class_ (ClassName "btn btn-danger"), HE.onClick $ toActionEvent Clear ]
                    [ HH.text "Clear plots" ]
                ]
            ]
        ]
    ]

toValueChangeActionEvent :: String -> Maybe Action
toValueChangeActionEvent value = Just $ HandleInput value

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots ExpressionManagerMessage m Unit
handleAction = case _ of
  HandleMessage plots -> do
    { selectedPlotId } <- H.get
    let
      newSelectedPlotId = if plotExists plots selectedPlotId then selectedPlotId else fromMaybe 0 $ (_.id) <$> (head plots)
    H.modify_ _ { plots = plots, selectedPlotId = newSelectedPlotId, editingSelected = false }
  Clear -> H.raise ClearPlots
  Add -> do
    { nextPlotId } <- H.get
    H.modify_ (_ { nextPlotId = nextPlotId + 1 })
    H.raise (AddPlot nextPlotId)
  Delete plotId -> do H.raise $ DeletePlot plotId
  Edit -> H.modify_ (_ { editingSelected = true })
  Rename -> do
    { selectedPlotId } <- H.get
    H.raise $ RenamePlot selectedPlotId "" -- TODO: retreieve input from text
  HandleInput value -> pure unit
  ChangeSelected plotId -> do
    { selectedPlotId, plots } <- H.get
    when ((plotExists plots plotId) && selectedPlotId /= plotId) do
      H.modify_ (_ { selectedPlotId = plotId, editingSelected = false })
  HandleExpressionInput message -> H.raise $ RaisedExpressionInputMessage message

toTab :: forall w. Boolean -> Boolean -> Int -> ExpressionPlot -> HH.HTML w Action
toTab allowDelete editingSelected selectedId plot =
  HH.li
    [ HP.class_ (ClassName "nav-item") ]
    [ HH.button
        [ HP.class_ (ClassName ("nav-link" <> maybeActiveClass))
        , (HE.onClick (toActionEvent (ChangeSelected plot.id)))
        ]
        ( input <> editButton <> cross
        )
    ]
  where
  isSelected = selectedId == plot.id

  addPaddingToInput = allowDelete || not editingSelected

  maybeActiveClass = if isSelected then " active" else ""

  text = if plot.expressionText == "" then "Plot " <> (show plot.id) else plot.expressionText

  input =
    if editingSelected then
      [ HH.input
          [ HP.type_ HP.InputText
          , HE.onFocusOut $ toActionEvent Rename
          , HE.onValueChange $ toValueChangeActionEvent
          , HP.value text
          , HP.class_ (ClassName $ if addPaddingToInput then "form-control small-input pr-2" else "form-control small-input")
          ]
      ]
    else
      [ HH.span
          [ HP.class_ (ClassName $ if addPaddingToInput then "pr-2" else "") ]
          [ HH.text text ]
      ]

  editButton =
    if isSelected && not editingSelected then
      [ HH.button
          [ HP.class_ (ClassName "close")
          , HE.onClick $ toActionEvent Edit
          ]
          [ HH.text "🖉" ]
      ]
    else
      []

  cross =
    if allowDelete then
      [ HH.button
          [ HP.class_ (ClassName "close")
          , HE.onClick $ toActionEvent (Delete plot.id)
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

plotExists :: Array ExpressionPlot -> Int -> Boolean
plotExists plots plotId = isJust (find (\p -> p.id == plotId) plots)

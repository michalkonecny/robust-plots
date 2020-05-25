module Main where

import Prelude

import Components.Canvas (Input, Slot, canvasComponent, xyBounds)
import Components.Canvas.CanvasController (canvasController)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (SProxy(..))
import Draw.Commands (DrawCommand)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Plot.Commands (PlotCommand, basicPlot, clear)
import Plot.Pan (pan)
import Plot.PlotController (computePlotAsync, toMaybePlotCommand)
import Plot.Zoom (zoom)
import Types (Direction(..), Size, XYBounds)

type Config
  = { someData :: String }

type State
  = { input :: Input (DrawCommand Unit)
    , bounds :: XYBounds
    , previousPlotCommand :: Maybe PlotCommand
    }

data Action
  = BasicPlot
  | Clear
  | Init
  | Pan Direction
  | Zoom Boolean 

type ChildSlots
  = ( canvas :: Slot Int )

_canvas = SProxy :: SProxy "canvas"

ui :: forall f i o. H.Component HH.HTML f i o (ReaderT Config Aff)
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
            { width: 800.0
            , height: 500.0
            }
        }
    , bounds: xyBounds (-1.0) (1.0) (-1.0) (1.0)
    , previousPlotCommand: Nothing
    }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Robust plot" ]
      , HH.button
          [ HE.onClick $ toActionEvent BasicPlot ]
          [ HH.text "Plot example function" ]
      , HH.button
          [ HE.onClick $ toActionEvent Clear ]
          [ HH.text "Clear plot" ]
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
      , HH.slot _canvas 1 (canvasComponent canvasController) state.input absurd
      ]

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

computePlot :: Size -> PlotCommand -> ReaderT Config Aff (DrawCommand Unit)
computePlot canvasSize plot = lift $ computePlotAsync canvasSize plot

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o (ReaderT Config Aff) Unit
handleAction action = do
  state <- H.get
  case action of
    Init -> do
      drawCommands <- lift $ computePlot state.input.size $ clear state.bounds
      H.put state { input { operations = drawCommands } }
    Clear -> do
      drawCommands <- lift $ computePlot state.input.size $ clear state.bounds
      H.put state { input { operations = drawCommands }, previousPlotCommand = Nothing }
    BasicPlot -> do
      let
        plot = basicPlot (isJust state.previousPlotCommand) state.bounds
      drawCommands <- lift $ computePlot state.input.size $ plot
      H.put state { input { operations = drawCommands }, previousPlotCommand = Just plot }
    Pan direction -> do
      let
        { plotCommand, newBounds } = pan state.bounds direction state.previousPlotCommand
      drawCommands <- lift $ computePlot state.input.size $ plotCommand
      H.put state { input { operations = drawCommands }, previousPlotCommand = toMaybePlotCommand plotCommand, bounds = newBounds }
    Zoom isZoomIn -> do
      let
        { plotCommand, newBounds } = zoom state.bounds isZoomIn state.previousPlotCommand
      drawCommands <- lift $ computePlot state.input.size $ plotCommand
      H.put state { input { operations = drawCommands }, previousPlotCommand = toMaybePlotCommand plotCommand, bounds = newBounds }

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (\app -> runReaderT app initialConfig) ui

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

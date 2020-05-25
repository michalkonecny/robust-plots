module Main where

import Prelude

import Components.Canvas (Input, Slot, canvasComponent, xyBounds)
import Components.Canvas.CanvasController (canvasController)
import Components.Canvas.Commands (DrawCommand)
import Components.Canvas.Plot (Plot, basicPlot, clear)
import Components.Canvas.PlotController (computePlotAsync)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
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
import Types (XYBounds, Size)

type Config
  = { someData :: String }

type State
  = { input :: Input (DrawCommand Unit)
    , bounds :: XYBounds
    , plotExists :: Boolean
    }

data Action
  = BasicPlot
  | Clear
  | Init

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
    , plotExists: false
    }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Robust plot" ]
      , HH.button
          [ HE.onClick \_ -> Just BasicPlot ]
          [ HH.text "Plot example function" ]
      , HH.button
          [ HE.onClick \_ -> Just Clear ]
          [ HH.text "Clear plot" ]
      , HH.slot _canvas 1 (canvasComponent canvasController) state.input absurd
      ]

computePlot :: Size -> Plot -> ReaderT Config Aff (DrawCommand Unit)
computePlot canvasSize plot = lift $ computePlotAsync canvasSize plot 

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o (ReaderT Config Aff) Unit
handleAction action = do
  state <- H.get
  case action of
    Init -> do
      plotCommands <- lift $ computePlot state.input.size $ clear $ state.bounds
      H.put state { input { operations = plotCommands } }
    Clear -> do
      plotCommands <- if state.plotExists 
        then lift $ computePlot state.input.size $ clear $ state.bounds
        else pure $ pure unit
      H.put state { input { operations = plotCommands }, plotExists = false }
    BasicPlot -> do
      plotCommands <- lift $ computePlot state.input.size $ basicPlot state.plotExists state.bounds
      H.put state { input { operations = plotCommands }, plotExists = true }

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (\app -> runReaderT app initialConfig) ui

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

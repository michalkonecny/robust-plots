module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Components.Canvas (Input, Slot, canvasComponent)
import Components.Canvas.Commands (DrawCommand)
import Components.Canvas.CanvasController (canvasController)
import Constants (canvasId)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

type Config
  = { }

type State
  = { input :: Input (DrawCommand Unit) }

data Action
  = BasicPlot

type ChildSlots
  = ( canvas :: Slot Int )

_canvas = SProxy :: SProxy "canvas"

ui :: forall f i o. H.Component HH.HTML f i o (ReaderT Config Aff)
ui =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction })
    }
  where
  initialState :: State
  initialState = { input: { operations: pure unit
  , canvasId: canvasId
  , size:
      { width: 800.0
      , height: 500.0
      }
  } }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Robust plot" ]
      , HH.button
          [ HE.onClick \_ -> Just BasicPlot ]
          [ HH.text "Plot polygon async" ]
      , HH.slot _canvas 1 (canvasComponent canvasController) state.input absurd
      ]

computePlot :: String -> ReaderT Config Aff (DrawCommand Unit)
computePlot q = do
  -- { } <- ask
  -- result <- lift (AX.get AXRF.string ("https://api.github.com/users/" <> q))
  pure $ pure unit

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o (ReaderT Config Aff) Unit
handleAction = case _ of
  BasicPlot -> do
    state <- H.get
    plotCommands <- lift (computePlot "kRITZCREEK")
    H.put state { input { operations = plotCommands } }

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (\app -> runReaderT app initialConfig) ui  

initialConfig :: Config
initialConfig = {}

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

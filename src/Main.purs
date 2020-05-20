module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Components.Canvas (Input, Slot, canvasComponent)
import Data.Symbol (SProxy(..))
import Constants (canvasId)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Components.Canvas.Context (renderer)
import Effect.Class (class MonadEffect)

type Config
  = { githubToken :: Maybe String }

type State
  = { userData :: Maybe String }

data Action
  = FetchData

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
  initialState = { userData: Nothing }

  render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Fetch user data" ]
      , HH.button
          [ HE.onClick \_ -> Just FetchData ]
          [ HH.text "Fetch" ]
      , HH.p_
          [ HH.text (fromMaybe "No user data" state.userData) ]
      , HH.slot _canvas 1 (canvasComponent renderer) input absurd
      ]

searchUser :: String -> ReaderT Config Aff String
searchUser q = do
  { githubToken } <- ask
  result <- case githubToken of
    Nothing -> lift (AX.get AXRF.string ("https://api.github.com/users/" <> q))
    Just token -> lift (AX.get AXRF.string ("https://api.github.com/users/" <> q <> "?access_token=" <> token))
  pure (either (const "") _.body result)

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o (ReaderT Config Aff) Unit
handleAction = case _ of
  FetchData -> do
    userData <- lift (searchUser "kRITZCREEK")
    H.put { userData: Just userData }

ui' :: forall f i o. H.Component HH.HTML f i o Aff
ui' = H.hoist (\app -> runReaderT app { githubToken: Nothing }) ui

input :: Input (Array String)
input =
  { operations: []
  , canvasId: canvasId
  , size:
      { width: 800.0
      , height: 500.0
      }
  }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui' unit body

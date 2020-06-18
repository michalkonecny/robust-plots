module Main where

import Prelude (Unit, bind, unit)
import Components.Main (mainComponent)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Components.Main.Types (Config)
import Control.Monad.Reader (runReaderT)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

mainComponent' :: forall f i o. H.Component HH.HTML f i o Aff
mainComponent' = H.hoist (\app -> runReaderT app initialConfig) mainComponent

initialConfig :: Config
initialConfig = { someData: "" }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI mainComponent' unit body

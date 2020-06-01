module Draw.Color where

import Prelude

data Color
  = RGBA
    { r :: Number
    , g :: Number
    , b :: Number
    , a :: Number
    }
  | HexCode String

instance colorShow :: Show Color where
  show (RGBA { r, g, b, a }) = "rgb(" <> (show r) <> "," <> (show g) <> "," <> (show b) <> "," <> (show a) <> ")"
  show (HexCode code) = code

rgba :: Number -> Number -> Number -> Number -> Color
rgba r g b a = RGBA { r, g, b, a }

hexCode :: String -> Color
hexCode code = HexCode code

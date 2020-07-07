module Components.BoundsInput where

import Prelude
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Expression.Parser (P, literal)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import IntervalArith.Misc (Rational, rationalToNumber)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.Expr (buildExprParser)
import Types (XYBounds)

type BoundsInputSlot p
  = forall q. H.Slot q BoundsInputMessage p

type State
  = { error :: Maybe String
    , oldBounds :: XYBounds
    , xBounds ::
        { upper :: String
        , lower :: String
        }
    , yBounds ::
        { upper :: String
        , lower :: String
        }
    }

data Bound
  = XLower
  | XUpper
  | YLower
  | YUpper

data BoundsInputMessage
  = UpdatedBoundsInput XYBounds

data Action
  = Recieve XYBounds
  | HandleInput Bound String
  | Update
  | ResetBounds

boundsInputComponent :: forall query m. MonadEffect m => H.Component HH.HTML query XYBounds BoundsInputMessage m
boundsInputComponent =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Recieve
              }
    }

initialState :: XYBounds -> State
initialState input =
  { error: Nothing
  , oldBounds: input
  , xBounds:
      { upper: show $ rationalToNumber input.xBounds.upper
      , lower: show $ rationalToNumber input.xBounds.lower
      }
  , yBounds:
      { upper: show $ rationalToNumber input.yBounds.upper
      , lower: show $ rationalToNumber input.yBounds.lower
      }
  }

render :: forall slots m. State -> HH.ComponentHTML Action slots m
render state =
  HH.div
    [ HP.class_ (ClassName "form-inline") ]
    $ [ HH.div
          [ HP.class_ (ClassName "pr-2") ]
          [ HH.input
              [ HP.type_ HP.InputText
              , HE.onValueChange $ toValueChangeActionEvent XLower
              , HP.value state.xBounds.lower
              , HE.onFocusOut $ toActionEvent Update
              , HP.class_ (ClassName "form-control small-input")
              ]
          , HH.span [ HP.class_ (ClassName "") ] [ HH.text " ≤ x ≤ " ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueChange $ toValueChangeActionEvent XUpper
              , HP.value state.xBounds.upper
              , HE.onFocusOut $ toActionEvent Update
              , HP.class_ (ClassName "form-control small-input")
              ]
          ]
      , HH.div
          [ HP.class_ (ClassName "pr-2") ]
          [ HH.input
              [ HP.type_ HP.InputText
              , HE.onValueChange $ toValueChangeActionEvent YLower
              , HP.value state.yBounds.lower
              , HE.onFocusOut $ toActionEvent Update
              , HP.class_ (ClassName "form-control small-input")
              ]
          , HH.span [ HP.class_ (ClassName "") ] [ HH.text " ≤ y ≤ " ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueChange $ toValueChangeActionEvent YUpper
              , HE.onFocusOut $ toActionEvent Update
              , HP.value state.yBounds.upper
              , HP.class_ (ClassName "form-control small-input")
              ]
          ]
      , HH.div
          [ HP.class_ (ClassName "btn-group") ]
          [ HH.button
              [ HP.class_ (ClassName "btn btn-warning"), HE.onClick $ toActionEvent $ ResetBounds ]
              [ HH.text "Reset bounds" ]
          ]
      ]
    <> (errorMessage state.error)

toValueChangeActionEvent :: Bound -> String -> Maybe Action
toValueChangeActionEvent bound value = Just $ HandleInput bound value

toActionEvent :: forall a. Action -> a -> Maybe Action
toActionEvent action _ = Just action

errorMessage :: forall slots m. Maybe String -> Array (HH.ComponentHTML Action slots m)
errorMessage Nothing = []

errorMessage (Just message) =
  [ HH.div
      [ HP.class_ (ClassName "alert alert-danger") ]
      [ HH.text message ]
  ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () BoundsInputMessage m Unit
handleAction = case _ of
  HandleInput XLower stringInput -> H.modify_ _ { xBounds { lower = stringInput } }
  HandleInput XUpper stringInput -> H.modify_ _ { xBounds { upper = stringInput } }
  HandleInput YLower stringInput -> H.modify_ _ { yBounds { lower = stringInput } }
  HandleInput YUpper stringInput -> H.modify_ _ { yBounds { upper = stringInput } }
  ResetBounds -> H.raise (UpdatedBoundsInput initialBounds)
  Recieve bounds ->
    H.modify_
      _
        { xBounds
          { upper = show $ rationalToNumber bounds.xBounds.upper
          , lower = show $ rationalToNumber bounds.xBounds.lower
          }
        , yBounds
          { upper = show $ rationalToNumber bounds.yBounds.upper
          , lower = show $ rationalToNumber bounds.yBounds.lower
          }
        , oldBounds = bounds
        }
  Update -> do
    { xBounds, yBounds, oldBounds } <- H.get
    let
      maybeXLower = parse xBounds.lower

      maybeXUpper = parse xBounds.upper

      maybeYLower = parse yBounds.lower

      maybeYUpper = parse yBounds.upper
    case maybeXLower of
      Nothing -> H.modify_ _ { error = Just $ "Failed to parse lower X bound" }
      Just xLower -> case maybeXUpper of
        Nothing -> H.modify_ _ { error = Just $ "Failed to parse upper X bound" }
        Just xUpper -> case maybeYLower of
          Nothing -> H.modify_ _ { error = Just $ "Failed to parse lower Y bound" }
          Just yLower -> case maybeYUpper of
            Nothing -> H.modify_ _ { error = Just $ "Failed to parse upper Y bound" }
            Just yUpper -> do
              let
                newBounds = xyBounds xLower xUpper yLower yUpper
              H.modify_ _ { error = Nothing }
              when (oldBounds /= newBounds)
                $ H.raise (UpdatedBoundsInput newBounds)

parse :: String -> Maybe Rational
parse input = case runParser input expressionParser of
  Left error -> Nothing
  Right number -> Just number

expressionParser :: P Rational
expressionParser = fix (\p -> buildExprParser [] literal)

initialBounds :: XYBounds
initialBounds = xyBounds (-one) one (-one) one

xyBounds :: Rational -> Rational -> Rational -> Rational -> XYBounds
xyBounds xLower xUpper yLower yUpper = { xBounds: { upper: xUpper, lower: xLower }, yBounds: { upper: yUpper, lower: yLower } }

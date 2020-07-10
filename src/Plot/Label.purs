module Plot.Label where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Free (resume)
import Data.Array (mapMaybe, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand, DrawCommandF(..))
import Misc.Maybe (toNothingIf)
import Types (Position)

type LabelledDrawCommand
  = Tuple String (DrawCommand Unit)

type LabelledPosition
  = Tuple String Position

drawRoughLabels :: (Position -> Boolean) -> Array LabelledDrawCommand -> DrawCommand Unit
drawRoughLabels isOffCanvas = drawLabels (toRoughLabelPosition isOffCanvas) isOffCanvas

drawLabels :: (DrawCommand Unit -> Maybe Position) -> (Position -> Boolean) -> Array LabelledDrawCommand -> DrawCommand Unit
drawLabels toLabelPosition isOffCanvas labelledCommands = pure unit -- TODO: draw labels
  where
  toLabelPositionWithText :: LabelledDrawCommand -> Maybe LabelledPosition
  toLabelPositionWithText = withLabelText toLabelPosition

  labelledPoints = mapMaybe toLabelPositionWithText labelledCommands

  placedLabelledPoints = placeLabel [] labelledPoints

  placeLabel :: Array LabelledPosition -> Array LabelledPosition -> Array LabelledPosition
  placeLabel placed [] = placed

  placeLabel placed toPlace = case uncons toPlace of
    Nothing -> placed
    Just { head, tail } -> placeLabel (placed <> [ repositionBasedOnPlaced placed head ]) tail

  repositionBasedOnPlaced :: Array LabelledPosition -> LabelledPosition -> LabelledPosition
  repositionBasedOnPlaced placed a@(Tuple text position) = a -- TODO: Fix label position

toRoughLabelPosition :: (Position -> Boolean) -> DrawCommand Unit -> Maybe Position
toRoughLabelPosition isOffCanvas = interpretWith interpretRough
  where
  interpretRough :: DrawCommandF (DrawCommand Unit) -> Maybe Position
  interpretRough = case _ of
    (ClearCanvas nextCommands) -> interpretWith interpretRough nextCommands
    (DrawText _ _ _ _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawXGridLine _ _ _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawYGridLine _ _ _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawXAxis _ _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawYAxis _ _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawPolygon _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawEnclosure _ _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawRootEnclosure _ _ _ nextCommands) -> interpretWith interpretRough nextCommands
    (DrawPlotLine a b nextCommands) -> mostLeft midPoint nextCommandsPosition
      where
      midPoint = toNothingIf isOffCanvas $ Just $ toMidPoint a b

      nextCommandsPosition = interpretWith interpretRough nextCommands

interpretWith :: forall a. (DrawCommandF (DrawCommand Unit) -> Maybe a) -> DrawCommand Unit -> Maybe a
interpretWith interpret commands = case resume commands of
  Right _ -> Nothing
  Left command -> interpret command

withLabelText :: forall a. (DrawCommand Unit -> Maybe a) -> LabelledDrawCommand -> Maybe (Tuple String a)
withLabelText interpret (Tuple text commands) = case interpret commands of
  Nothing -> Nothing
  Just a -> Just $ Tuple text a

mostLeft :: Maybe Position -> Maybe Position -> Maybe Position
mostLeft (Just a) (Just b) = if a.x < b.x then Just a else Just b

mostLeft a b = a <|> b

toMidPoint :: Position -> Position -> Position
toMidPoint a b = { x: (a.x + b.x) / 2.0, y: (a.y + b.y) / 2.0 }

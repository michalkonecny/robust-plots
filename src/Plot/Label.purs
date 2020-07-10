module Plot.Label
  ( toRoughLabelPosition
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Free (resume)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Draw.Commands (DrawCommand, DrawCommandF(..))
import Misc.Maybe (toNothingIf)
import Types (Position)

drawLabels :: (Position -> Boolean) -> Array (Tuple String (DrawCommand Unit)) -> DrawCommand Unit
drawLabels isOnCanvas labelledCommands = pure unit -- TODO: Fix label position and draw labels
  where
  labelledPoints = mapMaybe (withLabelText (toRoughLabelPosition isOnCanvas)) labelledCommands

withLabelText :: (DrawCommand Unit -> Maybe Position) -> Tuple String (DrawCommand Unit) -> Maybe (Tuple String Position)
withLabelText toPosition (Tuple text commands) = case toPosition commands of
  Nothing -> Nothing
  Just position -> Just $ Tuple text position

toRoughLabelPosition :: (Position -> Boolean) -> DrawCommand Unit -> Maybe Position
toRoughLabelPosition isOnCanvas = toLabelPosition interpretRough
  where 
    interpretRough :: DrawCommandF (DrawCommand Unit) -> Maybe Position
    interpretRough = case _ of
      (ClearCanvas nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawText _ _ _ _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawXGridLine _ _ _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawYGridLine _ _ _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawXAxis _ _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawYAxis _ _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawPolygon _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawEnclosure _ _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawRootEnclosure _ _ _ nextCommands) -> toLabelPosition interpretRough nextCommands
      (DrawPlotLine a b nextCommands) -> mostLeft midPoint nextCommandsPosition
        where
        midPoint = toNothingIf isOnCanvas $ Just $ toMidPoint a b

        nextCommandsPosition = toLabelPosition interpretRough nextCommands

toLabelPosition :: (DrawCommandF (DrawCommand Unit) -> Maybe Position) -> DrawCommand Unit -> Maybe Position
toLabelPosition interpret commands = case resume commands of
  Right _ -> Nothing
  Left command -> interpret command

mostLeft :: Maybe Position -> Maybe Position -> Maybe Position
mostLeft (Just a) (Just b) = if a.x < b.x then Just a else Just b

mostLeft a b = a <|> b

toMidPoint :: Position -> Position -> Position
toMidPoint a b = { x: (a.x + b.x) / 2.0, y: (a.y + b.y) / 2.0 }

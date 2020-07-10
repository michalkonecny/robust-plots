module Plot.Label
  ( toRoughLabelPosition
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Free (resume)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand, DrawCommandF(..))
import Misc.Maybe (toNothingIf)
import Types (Position)

toRoughLabelPosition :: (Position -> Boolean) -> DrawCommand Unit -> Maybe Position
toRoughLabelPosition isOnCanvas = toLabelPosition $ interpretRough isOnCanvas

toLabelPosition :: (DrawCommandF (DrawCommand Unit) -> Maybe Position) -> DrawCommand Unit -> Maybe Position
toLabelPosition interpret commands = case resume commands of
  Right _ -> Nothing
  Left command -> interpret command

interpretRough :: (Position -> Boolean) -> DrawCommandF (DrawCommand Unit) -> Maybe Position
interpretRough isOnCanvas = case _ of
  (ClearCanvas nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawText _ _ _ _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawXGridLine _ _ _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawYGridLine _ _ _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawXAxis _ _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawYAxis _ _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawPolygon _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawEnclosure _ _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawRootEnclosure _ _ _ nextCommands) -> toLabelPosition (interpretRough isOnCanvas) nextCommands
  (DrawPlotLine a b nextCommands) -> mostLeft midPoint nextCommandsPosition
    where
    midPoint = toNothingIf isOnCanvas $ Just $ toMidPoint a b

    nextCommandsPosition = toLabelPosition (interpretRough isOnCanvas) nextCommands

mostLeft :: Maybe Position -> Maybe Position -> Maybe Position
mostLeft (Just a) (Just b) = if a.x < b.x then Just a else Just b

mostLeft a b = a <|> b

toMidPoint :: Position -> Position -> Position
toMidPoint a b = { x: (a.x + b.x) / 2.0, y: (a.y + b.y) / 2.0 }

module Plot.Label
  ( toRoughLabelPosition
  ) where

import Prelude
import Control.Monad.Free (resume)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Draw.Commands (DrawCommand, DrawCommandF(..))
import Types (Position)

toRoughLabelPosition :: DrawCommand Unit -> Maybe Position
toRoughLabelPosition = toLabelPosition interpretRough

toLabelPosition :: (DrawCommandF (DrawCommand Unit) -> Maybe Position) -> DrawCommand Unit -> Maybe Position
toLabelPosition interpret commands = case resume commands of
  Right _ -> Nothing
  Left command -> interpret command

interpretRough :: DrawCommandF (DrawCommand Unit) -> Maybe Position
interpretRough (ClearCanvas nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawText _ _ _ _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawXGridLine _ _ _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawYGridLine _ _ _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawXAxis _ _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawYAxis _ _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawPolygon _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawEnclosure _ _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawRootEnclosure _ _ _ nextCommands) = toLabelPosition interpretRough nextCommands

interpretRough (DrawPlotLine a b nextCommands) = mostLeft (midPoint a b) $ toLabelPosition interpretRough nextCommands

mostLeft :: Position -> Maybe Position -> Maybe Position
mostLeft a Nothing = Just a

mostLeft a (Just b) = if a.x < b.x then Just a else Just b

midPoint :: Position -> Position -> Position
midPoint a b = { x: (a.x + b.x) / 2.0, y: (a.y + b.y) / 2.0 }

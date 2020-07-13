module Plot.Label where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Free (resume)
import Data.Array (any, fold, mapMaybe, uncons)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple (Tuple(..))
import Draw.Actions (drawText)
import Draw.Color (rgba)
import Draw.Commands (DrawCommand, DrawCommandF(..))
import Misc.Maybe (toNothingIf)
import Types (Position)

type LabelledDrawCommand
  = Tuple String (DrawCommand Unit)

type LabelledPosition
  = Tuple String Position

type BoundingBox
  = { size :: { width :: Number, height :: Number }, position :: Position }

textHeight :: Number
textHeight = 20.0

characterWidth :: Number
characterWidth = 10.0

drawRoughLabels :: (Position -> Boolean) -> Array LabelledDrawCommand -> DrawCommand Unit
drawRoughLabels isOffCanvas = drawLabels (toRoughLabelPosition isOffCanvas) isOffCanvas

drawLabels :: (DrawCommand Unit -> Maybe Position) -> (Position -> Boolean) -> Array LabelledDrawCommand -> DrawCommand Unit
drawLabels toLabelPosition isOffCanvas = drawAll <<< fixLabelledPositions isOffCanvas <<< mapMaybe toLabelPositionWithText
  where
  toLabelPositionWithText :: LabelledDrawCommand -> Maybe LabelledPosition
  toLabelPositionWithText = withLabelText toLabelPosition

drawAll :: Array LabelledPosition -> DrawCommand Unit
drawAll = fold <<< map draw

draw :: LabelledPosition -> DrawCommand Unit
draw (Tuple text position) = drawText color label textHeight position
  where
  color = rgba 255.0 0.0 0.0 1.0

  label = "f(x)=" <> text

fixLabelledPositions :: (Position -> Boolean) -> Array LabelledPosition -> Array LabelledPosition
fixLabelledPositions isOffCanvas = fixLabelledPositionsWith []
  where
  fixLabelledPositionsWith :: Array LabelledPosition -> Array LabelledPosition -> Array LabelledPosition
  fixLabelledPositionsWith fixed [] = fixed

  fixLabelledPositionsWith fixed toFix = case uncons toFix of
    Nothing -> fixed
    Just { head, tail } -> fixLabelledPositionsWith newFixed tail
      where
      newFixed = fixed <> [ reposition fixed head ]

  reposition :: Array LabelledPosition -> LabelledPosition -> LabelledPosition
  reposition [] labeledPosition = labeledPosition

  reposition fixed labeledPosition@(Tuple text _) = (Tuple text position)
    where
    fixedBoxes = map toSizeAndPosition fixed

    position = (repositionBox (toSizeAndPosition labeledPosition)).position

    repositionBox :: BoundingBox -> BoundingBox
    repositionBox box =
      if any (overlap box) fixedBoxes then
        repositionBox box { position { y = box.position.y - box.size.height } }
      else
        box

toRoughLabelPosition :: (Position -> Boolean) -> DrawCommand Unit -> Maybe Position
toRoughLabelPosition isOffCanvas = interpretWith interpretRough
  where
  interpretRough :: DrawCommandF (DrawCommand Unit) -> Maybe Position
  interpretRough = case _ of
    ClearCanvas nextCommands -> interpretWith interpretRough nextCommands
    DrawText _ _ _ _ nextCommands -> interpretWith interpretRough nextCommands
    DrawXGridLine _ _ _ nextCommands -> interpretWith interpretRough nextCommands
    DrawYGridLine _ _ _ nextCommands -> interpretWith interpretRough nextCommands
    DrawXAxis _ _ nextCommands -> interpretWith interpretRough nextCommands
    DrawYAxis _ _ nextCommands -> interpretWith interpretRough nextCommands
    DrawPolygon _ nextCommands -> interpretWith interpretRough nextCommands
    DrawEnclosure _ _ nextCommands -> interpretWith interpretRough nextCommands
    DrawRootEnclosure _ _ _ nextCommands -> interpretWith interpretRough nextCommands
    DrawPlotLine a b nextCommands -> mostLeft midPoint nextCommandsPosition
      where
      midPoint = toNothingIf isOffCanvas (Just (toMidPoint a b))

      nextCommandsPosition = interpretWith interpretRough nextCommands

----------------------------------------------------
-- Utility functions
----------------------------------------------------

interpretWith :: forall a. (DrawCommandF (DrawCommand Unit) -> Maybe a) -> DrawCommand Unit -> Maybe a
interpretWith interpret commands = case resume commands of
  Right _ -> Nothing
  Left command -> interpret command

withLabelText :: forall a. (DrawCommand Unit -> Maybe a) -> LabelledDrawCommand -> Maybe (Tuple String a)
withLabelText interpret (Tuple text commands) = case interpret commands of
  Nothing -> Nothing
  Just a -> Just (Tuple text a)

toSizeAndPosition :: LabelledPosition -> BoundingBox
toSizeAndPosition (Tuple text position) = { size: toSize text, position }

toSize :: String -> { width :: Number, height :: Number }
toSize text = { width: characterWidth * toNumber (length text), height: textHeight }

mostLeft :: Maybe Position -> Maybe Position -> Maybe Position
mostLeft (Just a) (Just b) = if a.x < b.x then Just a else Just b

mostLeft a b = a <|> b

toMidPoint :: Position -> Position -> Position
toMidPoint a b = { x: (a.x + b.x) / 2.0, y: (a.y + b.y) / 2.0 }

overlap :: BoundingBox -> BoundingBox -> Boolean
overlap box other = l && r && u && d
  where
  l = box.position.x < other.position.x + other.size.width

  r = box.position.x + box.size.width > other.position.x

  u = box.position.y < other.position.y + other.size.height

  d = box.position.y + box.size.height > other.position.y

module Components.Common.Action where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Events as HE
import Web.Event.Internal.Types (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

-- | A helper function to map the specified `const` action to an event handler function for a Halogen element.
toActionEvent :: forall action event. action -> event -> Maybe action
toActionEvent action = const $ Just action

-- | A helper function to map the specified `const` action to an `onValueChange` event handler 
-- | function for a Halogen element.
toValueChangeActionEvent :: forall action. (String -> action) -> String -> Maybe action
toValueChangeActionEvent action value = Just $ action value

-- | A helper function to map the specified `const` action to an `onChecked` event handler function 
-- | for a Halogen element. This helper function will ignore the event when the element is unchecked. 
toCheckedEvent :: forall action. action -> Boolean -> Maybe action
toCheckedEvent action true = Just action

toCheckedEvent _ false = Nothing

-- | Wrapper for the `onClick` event handler that will map the event to the specified `const` action.
onClickActionEvent :: forall action r. action -> IProp ( onClick :: MouseEvent | r ) action
onClickActionEvent action = HE.onClick $ toActionEvent action

-- | Wrapper for the `onFocusOut` event handler that will map the event to the specified `const` action.
onFocusOutActionEvent :: forall action r. action -> IProp ( onFocusOut :: FocusEvent | r ) action
onFocusOutActionEvent action = HE.onFocusOut $ toActionEvent action

-- | Wrapper for the `onValueChange` event handler that will map the event to the specified `const` action.
onValueChangeActionEvent :: forall action r. (String -> action) -> IProp ( onChange :: Event, value :: String | r ) action
onValueChangeActionEvent action = HE.onValueChange $ toValueChangeActionEvent action

-- | Wrapper for the `onChecked` event handler that will map the event to the specified `const` action. This 
-- | event handler will ignore the event when the element is unchecked.
onCheckedActionEvent :: forall action r. action -> IProp ( onChange :: Event, checked :: Boolean | r ) action
onCheckedActionEvent action = HE.onChecked $ toCheckedEvent action

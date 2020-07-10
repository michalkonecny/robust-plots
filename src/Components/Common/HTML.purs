module Components.Common.HTML where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toNonElementParentNode, HTMLDocument)
import Web.HTML.HTMLElement (HTMLElement, fromElement)
import Web.HTML.Window (document) as Web

findElementById :: String -> Effect HTMLElement
findElementById id = do
  document <- Web.document =<< Web.window
  findElementInDocumentById id document

findElementInDocumentById :: String -> HTMLDocument -> Effect HTMLElement
findElementInDocumentById id document = do
  maybeElement <- getElementById id $ toNonElementParentNode document
  case maybeElement of
    Nothing -> pure $ unsafeThrow $ "Cannot find element '" <> id <> "' in document"
    Just maybeHTMLElement -> case fromElement maybeHTMLElement of
      Nothing -> pure $ unsafeThrow $ "Cannot convert element '" <> id <> "' to HTMLElement"
      Just element -> pure $ element

module TreeSoup
where

import Prelude
import LenientHtmlParser
import Data.List (List (..), many)
import Control.Monad.State
import Control.Monad.Error.Class
import Data.Either (Either (..))
import Control.Alt
import Data.Generic
import Text.Parsing.Parser
import Text.Parsing.Parser.Pos
import Text.Parsing.Parser.Combinators
import Data.Foldable (elem, notElem)
import Data.String

data Node
  = TextNode String
  | Element String (List Attribute) (List Node)

derive instance genericNode :: Generic Node
derive instance eqNode :: Eq Node

instance showNode :: Show Node where
  show = gShow


unsoup :: List Tag -> Either ParseError (List Node)
unsoup tags = runParser tags pNodes

pNodes = many $ pNode AnyChild

pNode :: AllowedChildren -> Parser (List Tag) Node
pNode allowed = do
  t <- anyToken
  case t of
    TNode str -> do
      pure $ TextNode str
    TagSingle (TagName name) attrs -> do
      if isAllowedChild name allowed
        then pure $ Element name attrs Nil
        else fail $ "<" <> name <> "> not allowed here"
    TagOpen (TagName name) attrs -> do
      if isAllowedChild name allowed
        then pure $ Element name attrs Nil
        else fail $ "<" <> name <> "> not allowed here"
      children <- many <<< try $ pNode (allowedChildren name)
      optional <<< try $ pClose name
      pure $ Element name attrs children
    TagClose (TagName name) -> do
      fail $ "Unexpected closing tag </" <> name <> ">"

data AllowedChildren
  = NoChildrenAllowed
  | AnyChild
  | AnyChildIn (Array String)
  | AnyChildBut (Array String)

allowedChildren :: String -> AllowedChildren
allowedChildren = allowedChildren' <<< toLower

allowedChildren' :: String -> AllowedChildren
allowedChildren' "br" = NoChildrenAllowed
allowedChildren' "hr" = NoChildrenAllowed
allowedChildren' "link" = NoChildrenAllowed
allowedChildren' "meta" = NoChildrenAllowed
allowedChildren' "ul" = AnyChildIn ["li"]
allowedChildren' "ol" = AnyChildIn ["li"]
allowedChildren' "menu" = AnyChildIn ["li"]
allowedChildren' "li" = AnyChildBut ["li"]
allowedChildren' "p" = AnyChildBut ["p"]
allowedChildren' _ = AnyChild

isAllowedChildOf :: String -> String -> Boolean
isAllowedChildOf child parent =
  isAllowedChild child $ allowedChildren parent

isAllowedChild :: String -> AllowedChildren -> Boolean
isAllowedChild child allowed =
  case allowed of
    NoChildrenAllowed -> false
    AnyChild -> true
    AnyChildIn allowed -> toLower child `elem` allowed
    AnyChildBut prohibited -> toLower child `notElem` prohibited

pClose reqName = do
  t <- anyToken
  case t of
    TagClose (TagName name) ->
      if name == reqName
        then
          pure t
        else
          fail $ "Expected closing tag </" <> reqName <> ">, " <>
                 "but found </" <> name <> ">"
    x ->
      fail $ "Expected closing tag </" <> reqName <> ">, but found " <> show x

-- | Match any character.
anyToken :: forall a m. (Show a, Monad m) => ParserT (List a) m a
anyToken = do
  input <- gets \(ParseState input _ _) -> input
  case input of
    Nil ->
      fail "Unexpected EOF"
    Cons head tail -> do
      modify \(ParseState _ position _) ->
        ParseState tail
          (updatePosString position (show head))
          true
      pure head

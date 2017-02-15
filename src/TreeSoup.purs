module TreeSoup
where

import Prelude
import LenientHtmlParser (Attribute, Tag(..), TagName (..))
import Data.List (List (..), many)
import Control.Monad.State (gets, modify)
import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Text.Parsing.Parser (ParseError, ParseState (..), Parser, ParserT, fail, runParser)
import Text.Parsing.Parser.Pos (updatePosString)
import Text.Parsing.Parser.Combinators (optional, try)
import Data.Foldable (elem, notElem)
import Data.String (toLower)
import Text.HTML.Entities as Entities

data Node
  = TextNode String
  | Element String (List Attribute) (List Node)

derive instance genericNode :: Generic Node
derive instance eqNode :: Eq Node

instance showNode :: Show Node where
  show = gShow


unsoup :: List Tag -> Either ParseError (List Node)
unsoup tags = runParser tags pNodes

pNodes :: Parser (List Tag) (List Node)
pNodes = many $ pNode AnyChild

pNode :: AllowedChildren -> Parser (List Tag) Node
pNode allowed = do
  t <- anyToken
  case t of
    TNode str -> do
      pure <<< TextNode <<< Entities.decode $ str
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

-- | This lookup function is needed so that we can parse forgivingly, that is,
-- | we want to treat elements correctly for which the closing tag is optional.
allowedChildren' :: String -> AllowedChildren
-- <br>, <hr>, <link>, <meta> are always singletons:
allowedChildren' "br" = NoChildrenAllowed
allowedChildren' "hr" = NoChildrenAllowed
allowedChildren' "link" = NoChildrenAllowed
allowedChildren' "meta" = NoChildrenAllowed

-- so are images:
allowedChildren' "img" = NoChildrenAllowed

-- Lists (<ul>, <li>, <menu>) only allow <li> as children
allowedChildren' "ul" = AnyChildIn ["li"]
allowedChildren' "ol" = AnyChildIn ["li"]
allowedChildren' "menu" = AnyChildIn ["li"]

-- List items allow anything but other list items as children
allowedChildren' "li" = AnyChildBut ["li"]

-- Paragraphs auto-close on any block-level element
allowedChildren' "p" = AnyChildBut blockLevelElems

-- Tables are restricted:
allowedChildren' "table" = AnyChildIn ["thead", "tbody", "tfoot", "tr"]
allowedChildren' "tbody" = AnyChildIn ["tr"]
allowedChildren' "thead" = AnyChildIn ["tr"]
allowedChildren' "tfoot" = AnyChildIn ["tr"]
allowedChildren' "tr" = AnyChildIn ["td", "th"]
allowedChildren' "td" = AnyChildBut ["tr", "th", "td", "thead", "tbody", "tfoot"]
allowedChildren' "th" = AnyChildBut ["tr", "th", "td", "thead", "tbody", "tfoot"]

-- Definition lists are also restricted:
allowedChildren' "dl" = AnyChildIn ["dt", "dd"]
allowedChildren' "dt" = AnyChildBut ["dt", "dd"]
allowedChildren' "dd" = AnyChildBut ["dt", "dd"]

-- Any other element allows anything, as far as we are concerned
allowedChildren' _ = AnyChild

blockLevelElems :: Array String
blockLevelElems =
  [ "p"
  , "blockquote"
  , "code"
  , "pre"
  , "ul", "ol", "menu"
  , "li"
  , "table"
  , "tbody", "thead", "tfoot"
  , "tr", "th", "td"
  , "dl"
  , "dt", "dd"
  , "div", "section"
  , "form"
  , "textarea"
  , "h1", "h2", "h3", "h4", "h5", "h6"
  ]

isAllowedChildOf :: String -> String -> Boolean
isAllowedChildOf child parent =
  isAllowedChild child $ allowedChildren parent

isAllowedChild :: String -> AllowedChildren -> Boolean
isAllowedChild child =
  case _ of
    NoChildrenAllowed -> false
    AnyChild -> true
    AnyChildIn allowed -> toLower child `elem` allowed
    AnyChildBut prohibited -> toLower child `notElem` prohibited

pClose :: String -> Parser (List Tag) Unit
pClose reqName = do
  t <- anyToken
  case t of
    TagClose (TagName name) ->
      if name == reqName
        then
          pure unit
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

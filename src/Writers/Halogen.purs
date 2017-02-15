module Writers.Halogen
where

import Prelude
import LenientHtmlParser ( Attribute (..)
                         , Name (..)
                         , Value (..)
                         )
import TreeSoup (Node (..))
import Data.Foldable (intercalate, notElem)
import Data.Traversable (class Traversable, sequence)
import Data.List (List (..))
import Data.Tuple (Tuple (..))
import Data.String (toLower)
import Data.String.Utils (words)
import Data.Either (Either (..))
import Data.Int as Int
import Data.Maybe (maybe)
import Data.Array as Array

type HalogenOptions =
      { aliasH :: String
      , aliasP :: String
      , aliasE :: String
      , aliasC :: String
      }

defHalogenOptions :: HalogenOptions
defHalogenOptions =
  { aliasH: "HH"
  , aliasP: "HP"
  , aliasE: "HE"
  , aliasC: "HC"
  }

tagFn :: String -> String
tagFn x = toLower x

writeHalogen :: HalogenOptions
             -> List Node
             -> Either String String
writeHalogen opts nodes =
  intercalate "\n\n" <$> sequence (map (writeNode opts "") nodes)

writeNode :: HalogenOptions
             -> String
             -> Node
             -> Either String String
writeNode opts indent (TextNode t) =
  pure $ indent <> opts.aliasH <> ".text " <> show t
writeNode opts indent (Element tag attribs Nil) = do
  attrs <- writeAttributes opts attribs
  childrenStr <- if allowsChildren tag
    then
      pure "[]"
    else
      pure ""
  pure $ indent <> opts.aliasH <> "." <> tagFn tag <> " [" <> attrs <> "] " <> childrenStr
writeNode opts indent (Element tag attribs children) = do
  attrs <- writeAttributes opts attribs
  childrenStr <- if allowsChildren tag
    then
      ((\s -> "\n" <> indent <> "[\n" <> s <> "\n" <> indent <> "]") <<< intercalate ",\n")
        <$> sequence (map (writeNode opts (indent <> "  ")) children)
    else
      pure ""
  pure $ indent <> opts.aliasH <> "." <> tagFn tag <> " ["
                <> attrs
                <> "]"
                <> childrenStr



writeAttribute :: HalogenOptions -> Attribute -> Either String String
writeAttribute opts attr =
  translateAttribute opts attr >>= case _ of
    Tuple propfn args ->
      pure $ propfn <> " " <> intercalate " " args

writeAttributes :: forall f. Traversable f => HalogenOptions -> f Attribute -> Either String String
writeAttributes opts attrs =
  intercalate ", " <$> sequence (map (writeAttribute opts) attrs)

translateAttribute :: HalogenOptions -> Attribute -> Either String (Tuple String (Array String))
translateAttribute opts (Attribute (Name name) (Value val)) = do
  case toLower name of
    "class" ->
      case words val of
        [] -> pure $
          Tuple
            (opts.aliasP <> ".class_")
            [ "(" <> opts.aliasC <> ".ClassName " <> show "" <> ")" ]
        [className] -> pure $
          Tuple
            (opts.aliasP <> ".class_")
            [ "(" <> opts.aliasC <> ".ClassName " <> show className <> ")" ]
        classNames -> pure $
          Tuple
            (opts.aliasP <> ".classes")
            [ "[" <>
              (intercalate ", " $ flip map classNames $ \className ->
                opts.aliasC <> ".ClassName " <> show className
              ) <>
              "]"
            ]
    "alt" -> autoStr
    "charset" -> autoStr
    "cols" -> autoInt
    "rows" -> autoInt
    "colspan" -> named (opts.aliasP <> ".colSpan") <$> autoInt
    "rowspan" -> named (opts.aliasP <> ".rowSpan") <$> autoInt
    "for" -> named "htmlFor" <$> autoStr
    -- "width" -> autoPixel -- TODO
    -- "height" -> autoPixel -- TODO
    "href" -> autoStr
    "id" -> underscored <$> autoStr
    "name" -> autoStr
    "rel" -> autoStr
    "src" -> autoStr
    "target" -> autoStr
    "title" -> autoStr
    -- "method" -> autoFormMethod -- TODO
    "action" -> autoStr
    "novalidate" -> named (opts.aliasP <> ".noValidate") <$> autoBool
    -- "type" -> underscored <$> autoType -- TODO
    "value" -> autoStr
    "step" -> autoStr
    "disabled" -> autoBool
    "required" -> autoBool
    "readonly" -> named (opts.aliasP <> ".readOnly") <$> autoBool
    "spellcheck" -> named (opts.aliasP <> ".spellCheck") <$> autoBool
    "checked" -> autoBool
    "selected" -> autoBool
    "placeholder" -> autoStr
    "autocomplete" -> autoBool
    "multiple" -> autoBool
    "autoplay" -> autoBool
    "controls" -> autoBool
    "loop" -> autoBool
    "muted" -> autoBool
    "poster" -> autoStr
    -- "preload" -> autoPreload -- TODO
    "draggable" -> autoBool
    "tabindex" -> named (opts.aliasP <> ".tabIndex") <$> autoInt

    _ -> pure $
      Tuple
        (opts.aliasP <> ".prop " <>
         "(" <> opts.aliasC <> ".PropName " <> show name <> ")")
        [ show val ]
    where
      auto :: (String -> Either String (Array String)) -> Either String (Tuple String (Array String))
      auto convert = 
        Tuple <$> pure (opts.aliasP <> "." <> name)
              <*> convert val

      autoStr :: Either String (Tuple String (Array String))
      autoStr = auto (pure <<< Array.singleton <<< show)

      autoInt :: Either String (Tuple String (Array String))
      autoInt = auto $ \v -> do
        (Array.singleton <<< show) <$> maybe
          (Left $ "Invalid integer " <> v)
          Right
          (Int.fromString v)

      autoBool :: Either String (Tuple String (Array String))
      autoBool = auto $ \v -> do
        case v of
          "" -> pure [ "false" ]
          _ -> pure [ "true" ]

      named :: forall a. String -> Tuple String a -> Tuple String a
      named n (Tuple _ x) = Tuple n x

      underscored :: forall a. Tuple String a -> Tuple String a
      underscored (Tuple s x) = Tuple (s <> "_") x

allowsChildren :: String -> Boolean
allowsChildren tag = notElem tag childless
  where
    childless = ["br", "hr", "link", "meta", "img"]

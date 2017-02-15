module Writers.Halogen
where

import Prelude
import LenientHtmlParser ( Tag (..)
                         , Attribute (..)
                         , TagName (..)
                         , Name (..)
                         , Value (..)
                         )
import TreeSoup (Node (..))
import Data.Foldable (intercalate, traverse_)
import Data.Monoid (class Monoid, mempty)
import Data.List (List (..))
import Data.Tuple (Tuple (..))
import Data.String (toLower)

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
             -> String
writeHalogen opts nodes =
  intercalate "\n\n" $ map (writeNode opts "") nodes

writeNode :: HalogenOptions
             -> String
             -> Node
             -> String
writeNode opts indent (TextNode t) =
  indent <> opts.aliasH <> ".text " <> show t
writeNode opts indent (Element tag attribs Nil) =
  indent <> opts.aliasH <> "." <> tagFn tag <> " ["
    <> writeAttributes opts attribs
    <> "] []"
writeNode opts indent (Element tag attribs children) =
  indent <> opts.aliasH <> "." <> tagFn tag <> " ["
         <> writeAttributes opts attribs
         <> "] [\n"
         <> (intercalate ",\n" <<< map (writeNode opts (indent <> "  ")) $ children)
         <> "\n" <> indent <> "]"



writeAttribute :: HalogenOptions -> Attribute -> String
writeAttribute opts attr =
  case translateAttribute opts attr of
    Tuple propfn args ->
      propfn <> " " <> intercalate " " args

-- writeAttributes :: forall f. Traversable f => f Attribute -> String
writeAttributes opts = intercalate ", " <<< map (writeAttribute opts)

translateAttribute :: HalogenOptions -> Attribute -> Tuple String (Array String)
translateAttribute opts (Attribute name val) = 
  case name of
    _ -> Tuple
          (opts.aliasP <> ".prop " <> show name)
          [ "( " <> opts.aliasC <> ".PropName " <> show val <> " )" ]

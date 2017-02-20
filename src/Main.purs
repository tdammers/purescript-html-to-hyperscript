module Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (throw, catchException, message)
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Process (PROCESS)
import Node.Encoding (Encoding (..))
import Data.Either (Either (..))
import Data.String (trim)
import LenientHtmlParser (parseTags)
import TreeSoup (unsoup, Node, nodeToJson)
import Writers.Halogen (defHalogenOptions, writeHalogen)
import Ginger (GINGER, ginger)
import Data.List
import Data.Argonaut ( (:=), (~>), jsonEmptyObject )
import Data.Maybe

main :: forall e
      . Array String
      -> Eff ( console :: CONSOLE
             , fs :: FS
             , process :: PROCESS
             , interpretGinger :: GINGER
             | e
             ) Unit
main argv = catchException (error <<< message) do
  fn <- case argv of
    [] ->
      throw "Please provide a file name"
    [fn] ->
      pure fn
    xs ->
      throw "Please provide exactly one file name"
      
  src <- trim <$> readTextFile UTF8 fn
  tags <- case parseTags src of
    Left err ->
      throw ("Parser error: " <> show err)
    Right tags ->
      pure tags
  nodes <- case unsoup tags of
    Left err ->
      throw ("Error: cannot unsoup tags: " <> show err)
    Right nodes ->
      pure nodes
  -- case writeHalogen defHalogenOptions nodes of
  --   Left err ->
  --     throw ("Error: cannot convert to Halogen: " <> show err)
  --   Right output ->
  --     log output
  template <- readTextFile UTF8 "templates/halogen.tpl"
  writeTpl template nodes

writeTpl :: forall e
          . String
          -> List Node
          -> Eff (interpretGinger :: GINGER, console :: CONSOLE | e) Unit
writeTpl template nodes = do
  let context =  "nodes" := map nodeToJson nodes
              ~> jsonEmptyObject
  canceler <- runAff
    (const $ pure unit)
    (const $ pure unit) $ do
      result <- ginger template context
      liftEff' $ log result
  pure unit


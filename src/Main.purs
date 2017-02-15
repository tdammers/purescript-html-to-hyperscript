module Main where

import Prelude
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
import TreeSoup (unsoup)
import Writers.Halogen (defHalogenOptions, writeHalogen)

main :: forall e
      . Array String
      -> Eff ( console :: CONSOLE
             , fs :: FS
             , process :: PROCESS
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
  case writeHalogen defHalogenOptions nodes of
    Left err ->
      throw ("Error: cannot convert to Halogen: " <> show err)
    Right output ->
      log output

module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, error)
import Control.Monad.Eff.Exception (EXCEPTION, throw, catchException, message)
import LenientHtmlParser (Tag (..), parseTags)
import Node.FS (FS)
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (PROCESS, stdin)
import Node.Stream (readString, writeString)
import Node.Encoding (Encoding (..))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Foldable (intercalate, traverse_)
import Data.Monoid (class Monoid, mempty)
import Data.List (List (..))
import Data.String (trim)
import TreeSoup (unsoup, Node (..))

import Writers.Halogen

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
  log $ writeHalogen defHalogenOptions nodes

module Test.Main
where

import Prelude
import Node.Process
import Control.Monad.Eff

main :: forall e. Eff (process :: PROCESS | e) Unit
main = pure unit

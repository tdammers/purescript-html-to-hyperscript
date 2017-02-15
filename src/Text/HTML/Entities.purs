module Text.HTML.Entities
where

foreign import encode :: String -> String

foreign import decode :: String -> String

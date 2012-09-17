
module Types.Tag where

import Data.Record.StateFields

record "t" [d| data Tag = Tag {
    name :: String
  , isMood :: Bool
  , color :: Maybe String
  } deriving (Eq, Ord, Read, Show) |]


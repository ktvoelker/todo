
module Types.Entry where

import Data.Time.Clock
import Data.Record.StateFields

import Types.Core

record "e" [d| data Entry = Entry {
    title :: String
  , tags :: [Id]
  , due :: Maybe UTCTime
  , depends :: [Id]
  , externalType :: External
  , externalUrl :: String
  } deriving (Eq, Ord, Read, Show) |]


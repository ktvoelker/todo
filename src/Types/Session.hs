
module Types.Session where

import Data.Record.StateFields

import Types.Core

record "session" [d| data Session = Session {
    user :: Maybe Id
  } deriving (Eq, Ord, Read, Show) |]


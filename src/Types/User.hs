
module Types.User where

import Data.Record.StateFields

import qualified Data.Map as M

import Types.Core
import Types.Entry
import Types.Tag

record "up" [d| data UserProfile = UserProfile {
    name :: String
  } deriving (Eq, Ord, Read, Show) |]

record "ud" [d| data UserData = UserData {
    tags :: M.Map Id Tag
  , entries :: M.Map Id Entry
  } deriving (Eq, Ord, Read, Show) |]

record "u" [d| data User = User {
    id :: Id
  , password :: String
  , profile :: UserProfile
  , stuff :: UserData
  } |]


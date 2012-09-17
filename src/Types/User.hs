
module Types.User where

import Data.Record.StateFields

import qualified Data.Map as M

import Types.Core
import Types.Entry
import Types.Tag

record "u" [d| data User = User {
    name :: String
  , password :: String
  , tags :: M.Map Id Tag
  , entries :: M.Map Id Entry
  , id :: Id
  } deriving (Eq, Ord, Read, Show) |]


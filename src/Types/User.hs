
module Types.User where

import Data.Record.StateFields
import Data.Typeable

import qualified Data.Map as M

import Types.Core
import Types.Entry
import Types.Tag

record "u" [d| data User = User {
    name :: String
  , password :: String
  , tags :: M.Map Id Tag
  , entries :: M.Map Id Entry
  } deriving (Eq, Ord, Read, Show, Typeable) |]


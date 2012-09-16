
module Types.Database where

import Data.Record.StateFields
import Data.Typeable

import qualified Data.Map as M

import Types.Core
import Types.User

record "db" [d| newtype Database = Database {
    users :: M.Map Id User
  } deriving (Eq, Ord, Read, Show, Typeable) |]


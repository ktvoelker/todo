
module Types (
    module Data.Acid
  , module Data.Time.Clock
  , module Data.Record.StateFields
  , module Data.Record.StateFields.Containers
  , module Types
  , module Types.Core
  , module Types.Database
  , module Types.Entry
  , module Types.Request
  , module Types.Response
  , module Types.Session
  , module Types.Tag
  , module Types.User
  ) where

import qualified Data.Map as M

import Data.Acid
import Data.Time.Clock
import Data.Record.StateFields
import Data.Record.StateFields.Containers

import Types.Core
import Types.Database
import Types.Entry
import Types.JSON ()
import Types.Request
import Types.Response
import Types.SafeCopy ()
import Types.Session
import Types.Tag
import Types.User

defaultSession :: Session
defaultSession = Session { _sessionUser = Nothing }

emptyDatabase :: Database
emptyDatabase = Database { _dbUsers = M.empty }


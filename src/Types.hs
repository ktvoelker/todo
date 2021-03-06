
module Types (
    module Data.Acid
  , module Data.Time.Clock
  , module Data.Record.StateFields
  , module Data.Record.StateFields.Containers
  , module Web.Vorple
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
import Web.Vorple

import Types.Core
import Types.Database
import Types.Entry
import Types.Instances ()
import Types.JSON ()
import Types.Request
import Types.Response
import Types.SafeCopy ()
import Types.Session
import Types.Tag
import Types.User

type Handler = Vorple (AcidState Database) Session IO Response

defaultSession :: Session
defaultSession = Session { _sessionUser = Nothing }

emptyDatabase :: Database
emptyDatabase = Database { _dbUsers = M.empty }

emptyUser :: Id -> String -> String -> User
emptyUser id name password = User
  { _uId = id
  , _uPassword = password
  , _uProfile = UserProfile
    { _upName = name
    }
  , _uStuff = UserData
    { _udTags = M.empty
    , _udEntries = M.empty
    }
  }


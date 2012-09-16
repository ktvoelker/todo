
module Types (
    module Data.Acid
  , module Data.Time.Clock
  , module Data.Record.StateFields
  , module Data.Record.StateFields.Containers
  , module Types.Core
  , module Types.Database
  , module Types.Entry
  , module Types.Request
  , module Types.Tag
  , module Types.User
  ) where

import Data.Acid
import Data.Time.Clock
import Data.Record.StateFields
import Data.Record.StateFields.Containers

import Types.Core
import Types.Database
import Types.Entry
import Types.Request
import Types.SafeCopy ()
import Types.Tag
import Types.User

emptyQuery :: Query Database ()
emptyQuery = return ()

emptyUpdate :: Update Database ()
emptyUpdate = return ()

makeAcidic ''Database ['emptyQuery, 'emptyUpdate]

